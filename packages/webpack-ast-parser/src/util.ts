import { type ClassDeclaration, isAccessor, isConstructorDeclaration, isMethodDeclaration, isPropertyDeclaration, isSemicolonClassElement, type Node } from "typescript";

import { Position } from "@vencord-companion/shared/Position";
import { Range } from "@vencord-companion/shared/Range";

import type { AnyExportKey, ExportMap, RawExportMap } from "./types";
import { logger, WebpackAstParser } from "./WebpackAstParser";

export function allEntries<T extends object, K extends keyof T & (string | symbol)>(obj: T): (readonly [K, T[K]])[] {
    const SYM_NON_ENUMERABLE = Symbol("non-enumerable");
    const keys: (string | symbol)[] = Object.getOwnPropertyNames(obj);

    keys.push(...Object.getOwnPropertySymbols(obj));

    return keys.map((key) => {
        const descriptor = Object.getOwnPropertyDescriptor(obj, key);

        if (!descriptor)
            throw new Error("Descriptor is undefined");

        if (!descriptor.enumerable)
            return SYM_NON_ENUMERABLE;

        return [key as K, (obj as any)[key] as T[K]] as const;
    })
        .filter((x) => x !== SYM_NON_ENUMERABLE);
}

export function fromEntries<T extends Object>(entries: Iterable<readonly [keyof T, T[keyof T]]>): T {
    return Object.fromEntries(entries) as any;
}

export function allValues<T extends object>(obj: T): (T[keyof T])[] {
    return allEntries(obj)
        .map(([, v]) => v);
}

export function allKeys<T extends object>(obj: T): (keyof T)[] {
    return allEntries(obj)
        .map(([k]) => k);
}

export function containsPosition(range: ExportMap<Range> | Range[], pos: Position): boolean {
    if (Array.isArray(range)) {
        return range.some((r) => r.contains(pos));
    }
    return allValues(range)
        .filter((r) => typeof r !== "string" && r != null)
        .some((r) => containsPosition(r, pos));
}

/**
 * @param text the module text
 * @returns if the module text is a webpack module or an extracted find
 */
export function isWebpackModule(text: string) {
    return text.startsWith("// Webpack Module ")
      || text.substring(0, 100)
          .includes("//OPEN FULL MODULE:");
}

/**
 * **does not** format the modules code see {@link format} for more code formating

 * takes the raw contents of a module and prepends a header
 * @param moduleContents the module
 * @param moduleId the module id
 * @param isFind if the module is coming from a find
    eg: is it a partial module
 * @returns a string with the formatted module
 */

export function formatModule(moduleContents: string, moduleId: string | number | undefined = "000000", isFind?: boolean): string {
    if (isFind)
        return `// Webpack Module ${moduleId} \n${isFind ? `//OPEN FULL MODULE: ${moduleId}\n` : ""}//EXTRACED WEPBACK MODULE ${moduleId}\n 0,\n${moduleContents}`;
    return moduleContents;
}

export function TAssert<T>(thing: any): asserts thing is T {
}

export function assertNotHover<T>(thing: ExportMap<T>[keyof ExportMap<T>]):
    asserts thing is Exclude<ExportMap<T>[keyof ExportMap<T>], ExportMap<T>[typeof WebpackAstParser.SYM_HOVER]> {

}
/**
 * @internal
 */
export function getNestedExportFromMap<T>(keys: readonly AnyExportKey[], map: ExportMap<T>): T[] | undefined {
    let i = 0;
    let cur: ExportMap<T>[keyof ExportMap<T>] = map;

    while ((cur = cur[keys[i++]])) {
        if (Array.isArray(cur)) {
            return cur;
        } else if (Array.isArray(cur[WebpackAstParser.SYM_CJS_DEFAULT])) {
            // @ts-expect-error i just fucking checked this typescript
            return cur[WebpackAstParser.SYM_CJS_DEFAULT];
        }
    }
    return undefined;
}
/**
 * @internal
 * 
 * @returns ```js
 * {
 *     "<PASSED_IN_CLASS_NAME>": {
 *          [WebpackAstParser.SYM_CJS_DEFAULT]: ["<CONSTRUCTOR>"],
 *          ["methodName"]: ["METHOD"]
 *     }
 * }
 * ```
 */
export function parseClassDeclaration(clazz: ClassDeclaration, extraExportRanges: Node[] = []): RawExportMap {
    const ret: RawExportMap = {
        [WebpackAstParser.SYM_CJS_DEFAULT]: [...extraExportRanges, clazz.name ?? clazz.getChildAt(0)],
    };

    for (const member of clazz.members) {
        if (isMethodDeclaration(member)) {
            if (!member.body)
                continue;
            ret[member.name.getText()] = [member.name];
        } else if (isConstructorDeclaration(member)) {
            // the ConstructoKeyword
            const arr = ret[WebpackAstParser.SYM_CJS_DEFAULT];

            if (!Array.isArray(arr)) {
                logger.error("CJS default export is not an array, this should be never happen");
                continue;
            }
            arr.push(member.getChildAt(0));
        } else if (isPropertyDeclaration(member)) {
            ret[member.name.getText()] = [member.name];
        } else if (isAccessor(member)) {
            if (!member.body)
                continue;
            ret[member.name.getText()] = [member.name];
        } else if (isSemicolonClassElement(member)) {
            // ignore this
        } else {
            logger.warn("Unhandled class member type. This should be handled");
        }
    }

    // name ?? ClassKeyword
    ret[WebpackAstParser.SYM_CJS_DEFAULT] ??= [clazz.name ?? clazz.getChildAt(0)];

    return ret;
}

