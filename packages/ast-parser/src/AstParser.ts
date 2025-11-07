import { Format } from "@sadan4/devtools-pretty-printer";
import { collectVariableUsage, type VariableInfo } from "ts-api-utils";
import {
    type AssignmentExpression,
    type AssignmentOperatorToken,
    type CallExpression,
    createSourceFile,
    type Expression,
    type Identifier,
    isBinaryExpression,
    isFunctionLike,
    isIdentifier,
    isVariableDeclaration,
    type Node,
    type ReadonlyTextRange,
    ScriptKind,
    ScriptTarget,
    type SourceFile,
    SyntaxKind,
    type VariableDeclaration,
} from "typescript";

import { Cache, CacheGetter } from "@vencord-companion/shared/decorators";
import { type Logger, NoopLogger } from "@vencord-companion/shared/Logger";
import { type IPosition, Position } from "@vencord-companion/shared/Position";
import { Range } from "@vencord-companion/shared/Range";

import type { StringifiedModule } from "./StringifiedModule";
import type { Functionish } from "./types";
import { CharCode, getTokenAtPosition, isAssignmentExpression, isConstDeclared, isEOL } from "./util";

/**
 * @internal
 */
export let logger: Logger = NoopLogger;

export function setLogger(newLogger: Logger) {
    logger = newLogger;
}

export class AstParser {
    public static withFormattedText(text: string) {
        return new this(Format(text));
    }

    public readonly text: string;

    /**
     * @CacheGetter
     */
    @CacheGetter()
    public get sourceFile(): SourceFile {
        return this.createSourceFile();
    }

    /**
     * All the variables in the source file
     * @CacheGetter
     */
    @CacheGetter()
    public get vars(): Map<Identifier, VariableInfo> {
        return collectVariableUsage(this.sourceFile);
    }

    /**
     * @CacheGetter
     */
    @CacheGetter()
    public get usesToVars(): Map<Identifier, VariableInfo> {
        const map = new Map<Identifier, VariableInfo>();

        for (const [, info] of this.vars) {
            for (const { location } of info.uses) {
                map.set(location, info);
            }
            // for (const decl of info.declarations) {
            //     map.set(decl, info);
            // }
        }

        return map;
    }

    public getVarInfoFromUse(ident: Identifier): VariableInfo | undefined {
        return this.usesToVars.get(ident);
    }

    // FIXME: add tests for this
    /**
     * @param use a use of a variable
     * @param decl a declaration of a variable
     * @returns true of the use is a use of the declaration, false otherwise
     */
    public isUseOf(use: Identifier | undefined, decl: Identifier | undefined): boolean {
        if (!decl || !use)
            return false;

        const varInfo = this.vars.get(decl);

        if (!varInfo)
            return false;

        const varInfoFromUse = this.usesToVars.get(use);

        return varInfoFromUse === varInfo;
    }

    public constructor(text: string) {
        this.text = text;
    }

    /**
     * given something like this
     * ```js
     * const bar = "foo";
     * const baz = bar;
     * const qux = baz;
     * ```
     * if given `qux` it will return `[bar, baz]`;
     *
     * fails on something where a variable is reassigned
     */
    public unwrapVariableDeclaration(ident: Identifier): Identifier[] | undefined {
        const arr: Identifier[] = [];
        let last = ident;

        while (true) {
            const [varDec, ...rest] = this.getVarInfoFromUse(last)?.declarations ?? [];

            if (!varDec)
                break;
            if (rest.length) {
                arr.length = 0;
                break;
            }
            arr.push(last = varDec);
        }
        if (arr.length !== 0)
            return arr;
        logger.debug("[AstParser] Failed finding variable declaration");
    }

    public isCallExpression(node: Node | undefined): node is CallExpression {
        return node?.kind === SyntaxKind.CallExpression;
    }

    /**
     * Used for interop with other systems
     */
    // FIXME: PACKAGE -
    public serialize(): StringifiedModule {
        return {
            content: this.text,
        } satisfies StringifiedModule;
    }

    /**
     * given the `x` of
     * ```js
     * const x = {
     * foo: bar
     * }
     * ```
     * NOTE: this must be the exact x, not a use of it
     * @returns the expression {foo: bar}
     */
    public getVariableInitializer(ident: Identifier): Expression | undefined {
        const dec = ident.parent;

        if (!isVariableDeclaration(dec))
            return;
        return dec.initializer;
    }

    public isVariableAssignmentLike(node: Node | undefined):
    node is
    | (
      & Omit<VariableDeclaration, "name" | "initializer">
      & {
          name: Identifier;
          initializer: Exclude<VariableDeclaration["initializer"], undefined>;
      }
    )
    | (Omit<AssignmentExpression<AssignmentOperatorToken>, "left"> & { left: Identifier; }) {
        if (!node)
            return false;

        if (isVariableDeclaration(node)) {
            return isIdentifier(node.name) && !!node.initializer;
        } else if (isBinaryExpression(node)) {
            return isAssignmentExpression(node);
        }
        return false;
    }

    /**
     * given a variable, if it has a single assignment in this file, return the expression assigned to it
     * 
     * returns undefined if there are multiple assignments, or if the variable is assigned more than once
     */
    public findSingleAssignment(info: VariableInfo): Expression | undefined {
        const { declarations, uses } = info;

        if (declarations.length !== 1) {
            logger.warn("[AstParser] findSingleAssignment: multiple declarations");
            return;
        }

        const [decl] = declarations;

        if (isConstDeclared(info)) {
            const init = this.getVariableInitializer(decl);

            if (!init) {
                logger.warn("[AstParser] findSingleAssignment: const variable without initializer");
            }
            return init;
        }

        let init: Expression | undefined;

        for (const { location } of uses) {
            if (isAssignmentExpression(location.parent)) {
                // filter out cases like `<some other thing> = location`
                if (location.parent.left !== location) {
                    continue;
                }
                if (init || location.parent.operatorToken.kind !== SyntaxKind.EqualsToken) {
                    return;
                }
                init = location.parent.right;
            }
        }

        return init;
    }

    /**
     * Create the source file for this parser
     *
     * MUST SET PARENT NODES
     * @Cache
     */
    @Cache()
    protected createSourceFile(): SourceFile {
        return createSourceFile(
            "file.tsx",
            this.text,
            ScriptTarget.ESNext,
            true,
            ScriptKind.TSX,
        );
    }

    /** Returns the token at or following the specified position or undefined if none is found inside `parent`. */
    public getTokenAtOffset(pos: number): Node | undefined {
        return getTokenAtPosition(this.sourceFile, pos, this.sourceFile, false);
    }

    public getTokenAtPosition(pos: IPosition): Node | undefined {
        return this.getTokenAtOffset(this.offsetAt(pos));
    }

    /**
     * convert two offsets to a range
     * 
     * **DO NOT USE WITH AN AST NODE, IT WILL LEAD TO INCORRECT LOCATIONS**
     * @see makeRangeFromAstNode
     */
    public makeRange({ pos, end }: ReadonlyTextRange): Range {
        return new Range(this.positionAt(pos), this.positionAt(end));
    }

    public makeRangeFromAstNode(node: Node) {
        return new Range(this.positionAt(node.getStart(this.sourceFile)), this.positionAt(node.end));
    }

    public makeRangeFromAnonFunction(func: Functionish): Range {
        const { pos } = func.body ?? { pos: func.getEnd() };

        return this.makeRange({
            pos: func.getStart(),
            end: pos,
        });
    }

    public makeRangeFromFunctionDef(ident: Identifier): Range | undefined {
        const { declarations } = this.getVarInfoFromUse(ident) ?? {};

        if (!declarations) {
            logger.debug("makeRangeFromFunctionDef: no declarations found for identifier");
            return undefined;
        }
        if (declarations.length !== 1) {
            logger.debug("makeRangeFromFunctionDef: zero or multiple declarations found for identifier");
            return undefined;
        }
        if (declarations[0].parent && !isFunctionLike(declarations[0].parent)) {
            logger.debug("makeRangeFromFunctionDef: dec. parent is not a function");
            return undefined;
        }
        return this.makeRangeFromAstNode(declarations[0]);
    }

    /**
     * Converts the position to a zero-based offset.
     * Invalid positions are adjusted as described in {@link Position.line}
     * and {@link Position.character}.
     *
     * @param position A position.
     * @return A valid zero-based offset.
     */
    // copied from vscode-languageserver-node
    public offsetAt(position: IPosition): number {
        const { lineOffsets } = this;

        if (position.line >= lineOffsets.length) {
            return this.text.length;
        } else if (position.line < 0) {
            return 0;
        }

        const lineOffset = lineOffsets[position.line];

        if (position.character <= 0) {
            return lineOffset;
        }

        const nextLineOffset
            = position.line + 1 < lineOffsets.length
                ? lineOffsets[position.line + 1]
                : this.text.length;

        const offset = Math.min(lineOffset + position.character, nextLineOffset);

        return this.ensureBeforeEOL(offset, lineOffset);
    }

    // methods copied from vscode-languageserver-node
    /**
     * @CacheGetter
     */
    @CacheGetter()
    private get lineOffsets() {
        return this.computeLineOffsets(true);
    }

    @CacheGetter()
    /**
     * @CacheGetter
     */
    public get lineCount() {
        return this.lineOffsets.length;
    }

    private ensureBeforeEOL(offset: number, lineOffset: number): number {
        while (offset > lineOffset && isEOL(this.text.charCodeAt(offset - 1))) {
            offset--;
        }
        return offset;
    }

    private computeLineOffsets(isAtLineStart: boolean, textOffset = 0): number[] {
        const { text } = this;
        const result: number[] = isAtLineStart ? [textOffset] : [];

        for (let i = 0; i < text.length; i++) {
            const ch = text.charCodeAt(i);

            if (isEOL(ch)) {
                if (
                    ch === CharCode.CarriageReturn
                    && i + 1 < text.length
                    && text.charCodeAt(i + 1) === CharCode.LineFeed
                ) {
                    i++;
                }
                result.push(textOffset + i + 1);
            }
        }
        return result;
    }

    /**
     * Converts a zero-based offset to a position.
     *
     * @param offset A zero-based offset.
     * @return A valid {@link Position position}.
     * @example The text document "ab\ncd" produces:
     * position { line: 0, character: 0 } for `offset` 0.
     * position { line: 0, character: 1 } for `offset` 1.
     * position { line: 0, character: 2 } for `offset` 2.
     * position { line: 1, character: 0 } for `offset` 3.
     * position { line: 1, character: 1 } for `offset` 4.
     */
    public positionAt(offset: number): Position {
        offset = Math.max(Math.min(offset, this.text.length), 0);

        const { lineOffsets } = this;

        let low = 0,
            high = lineOffsets.length;

        if (high === 0) {
            return new Position(0, offset);
        }
        while (low < high) {
            const mid = Math.floor((low + high) / 2);

            if (lineOffsets[mid] > offset) {
                high = mid;
            } else {
                low = mid + 1;
            }
        }

        // low is the least x for which the line offset is larger than the current offset
        // or array.length if no line offset is larger than the current offset
        const line = low - 1;

        offset = this.ensureBeforeEOL(offset, lineOffsets[line]);
        return new Position(line, offset - lineOffsets[line]);
    }
}
