import {
    createPrinter,
    EmitHint,
    isArrowFunction,
    isFunctionExpression,
    isPropertyAssignment,
    isRegularExpressionLiteral,
    isStringLiteral,
    type Node,
    type ObjectLiteralExpression,
    ScriptTarget,
    transpileModule,
} from "typescript";

import { findObjectLiteralByKey } from "@vencord-companion/ast-parser/util";

import type { FunctionNode, IFindType, RegexNode, StringNode } from "./types";


export function tryParseStringLiteral(node: Node): StringNode | null {
    if (!isStringLiteral(node))
        return null;

    return {
        type: "string",
        value: node.text,
    };
}

export function tryParseRegularExpressionLiteral(node: Node): RegexNode | null {
    if (!isRegularExpressionLiteral(node))
        return null;

    const m = node.text.match(/^\/(.+)\/(.*?)$/);

    return m && {
        type: "regex",
        value: {
            pattern: m[1],
            flags: m[2],
        },
    };
}

export function parseFind(patch: ObjectLiteralExpression): IFindType | null {
    const find = findObjectLiteralByKey(patch, "find");

    if (!find || !isPropertyAssignment(find))
        return null;
    if (!(isStringLiteral(find.initializer) || isRegularExpressionLiteral(find.initializer)))
        return null;

    return {
        findType: isStringLiteral(find.initializer) ? "string" : "regex",
        find: find.initializer.text,
    };
}

/**
 * @internal
 */
export function tryParseFunction(node: Node): FunctionNode | null {
    if (!isArrowFunction(node) && !isFunctionExpression(node))
        return null;

    const code = createPrinter()
        .printNode(EmitHint.Expression, node, node.getSourceFile());

    const res = transpileModule(code, {
        compilerOptions: {
            target: ScriptTarget.ESNext,
            strict: true,
        },
    });

    if (res.diagnostics && res.diagnostics.length > 0)
        return null;

    return {
        type: "function",
        value: res.outputText,
    };
}
