import type { VariableInfo } from "ts-api-utils";
import { type Expression, isArrowFunction, isElementAccessExpression, isObjectLiteralExpression, isPropertyAccessExpression, isPropertyAssignment, isStringLiteralLike, isVariableDeclaration, type ObjectLiteralElementLike, type ObjectLiteralExpression, type PropertyName } from "typescript";

import type { Functionish } from "@vencord-companion/ast-parser/types";
import { findParent, isAssignmentExpression, isBinaryPlusExpression, isFunctionish, lastChild, nonNull, tryParseStringOrNumberLiteral } from "@vencord-companion/ast-parser/util";
import { Cache, CacheGetter } from "@vencord-companion/shared/decorators";

import type { JSHashEntry } from "./types";
import { WebpackChunkParser } from "./WebpackChunkParser";

const BUILD_MODULE_REGEX = /Trying to open a changelog for an invalid build number/;
const BUILD_NUMBER_REGEX = /(?:parseInt\("|"Trying to open a changelog for an invalid build number )(\d+?)"\)/;
const KNOWN_BUILD_MODULE_IDS: ReadonlyArray<string> = Object.freeze(["128014", "446023"]);

export class WebpackMainChunkParser extends WebpackChunkParser {
    @CacheGetter()
    get __webpack_require__(): VariableInfo | undefined {
        for (const [ident, info] of this.vars) {
            if (ident.text === "__webpack_require__") {
                return info;
            }
        }
    }

    @CacheGetter()
    get __webpack_modules__(): VariableInfo | undefined {
        for (const [ident, info] of this.vars) {
            if (ident.text === "__webpack_modules__") {
                return info;
            }
        }
    }

    @Cache()
    public getJsChunkHashes(): JSHashEntry[] {
        const uses = this.__webpack_require__?.uses;

        if (!uses) {
            return [];
        }

        let uFunc: Functionish | undefined;

        foundU: {
            for (const { location: { parent } } of uses) {
                if (!isPropertyAccessExpression(parent)) {
                    continue;
                }

                // webpack js chunk name->id function
                if (parent.name.text !== "u") {
                    continue;
                }

                const maybeAssign = parent.parent;

                if (!isAssignmentExpression(maybeAssign) || !isFunctionish(maybeAssign.right)) {
                    continue;
                }

                uFunc = maybeAssign.right;
                break foundU;
            }
            return [];
        }

        if (!isArrowFunction(uFunc)) {
            return [];
        }

        const { body: ret } = uFunc;

        // expect body to be BinExp>[BinExp>["" + {id:hash}[id]] + ".js"]
        if (!isBinaryPlusExpression(ret)) {
            return [];
        }

        const { left: concatWithHashMap } = ret;

        if (!isBinaryPlusExpression(concatWithHashMap)) {
            return [];
        }

        // {id:hash}[id]
        const { right: hashMapAccess } = concatWithHashMap;

        if (!isElementAccessExpression(hashMapAccess)) {
            return [];
        }

        const hashMap = lastChild(hashMapAccess.expression, isObjectLiteralExpression);

        if (!hashMap) {
            return [];
        }

        return hashMap
            .properties
            .map(this.parseHashMapEntry.bind(this))
            .filter(nonNull);
    }

    @Cache()
    public getBuildNumber(): string | undefined {
        const m = this.getDefinedModules();

        if (!m) {
            return;
        }

        for (const maybeId of KNOWN_BUILD_MODULE_IDS) {
            const moduleText = m[maybeId];

            if (BUILD_MODULE_REGEX.test(moduleText)) {
                const [, id] = BUILD_NUMBER_REGEX.exec(moduleText) ?? [];

                if (!id) {
                    return;
                }

                return id;
            }
        }
    }

    protected tryParseHashMapKey(node: PropertyName): string | undefined {
        return tryParseStringOrNumberLiteral(node);
    }

    private tryParseHashMapValue(node: Expression): string | undefined {
        if (!isStringLiteralLike(node)) {
            return;
        }
        return node.text;
    }

    private parseHashMapEntry(node: ObjectLiteralElementLike): JSHashEntry | undefined {
        if (!isPropertyAssignment(node)) {
            return;
        }

        const id = this.tryParseHashMapKey(node.name);

        if (!id) {
            return;
        }

        const hash = this.tryParseHashMapValue(node.initializer);

        if (!hash) {
            return;
        }

        return [id, hash];
    }

    @Cache()
    override getModuleObject(): ObjectLiteralExpression | undefined {
        const wpModules = this.__webpack_modules__;

        if (!wpModules) {
            return;
        }

        const { declarations } = wpModules;

        if (declarations.length !== 1) {
            return;
        }

        const wpModulesDecl = findParent(declarations[0], isVariableDeclaration)?.initializer;

        if (!wpModulesDecl || !isObjectLiteralExpression(wpModulesDecl)) {
            return;
        }

        return wpModulesDecl;
    }
}
