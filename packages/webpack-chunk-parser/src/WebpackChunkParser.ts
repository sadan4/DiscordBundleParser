import { isMethodDeclaration, isPropertyAssignment, type MethodDeclaration, type ObjectLiteralElementLike, type ObjectLiteralExpression, type PropertyAssignment } from "typescript";

import { AstParser, isFunctionish, nonNull, tryParseStringOrNumberLiteral } from "@vencord-companion/ast-parser";
import { Cache } from "@vencord-companion/shared/decorators";

import type { ModuleEntry } from "./types";

function fromEntries<
    K extends PropertyKey,
    V,
>(obj: Record<K, V>, [k, v]: NoInfer<readonly [K, V]>, _idx: number, _arr: readonly [K, V][]): Record<K, V> {
    obj[k] = v;
    return obj;
}

export abstract class WebpackChunkParser extends AstParser {
    /**
     * @retuns the object with each module defined, should conform to Record<PropertyKey, (e, t, n) => void)>
     */
    abstract getModuleObject(): ObjectLiteralExpression | undefined;

    /**
     * ```js
     * let __webpack_modules__ = {
     *     123: function(module, exports, require) {
     *         // module
     *     }
     * };
     */
    private tryParseChunkEntryPropertyAssignment(entry: PropertyAssignment): ModuleEntry | undefined {
        const moduleId = tryParseStringOrNumberLiteral(entry.name);

        if (!moduleId) {
            return;
        }

        const moduleValue = entry.initializer;

        if (!isFunctionish(moduleValue)) {
            return;
        }

        return [moduleId, moduleValue.getText()];
    }

    /**
     * ```js
     * let __webpack_modules__ = {
     *     123(module, exports, require) {
     *         // module
     *     }
     * };
     * ```
     */
    private tryParseChunkEntryMethodDecl(entry: MethodDeclaration): ModuleEntry | undefined {
        const moduleId = tryParseStringOrNumberLiteral(entry.name);

        if (!moduleId) {
            return;
        }
        return [moduleId, entry.getText()];
    }

    private tryParseChunkEntry(entry: ObjectLiteralElementLike): ModuleEntry | undefined {
        if (isPropertyAssignment(entry)) {
            return this.tryParseChunkEntryPropertyAssignment(entry);
        } else if (isMethodDeclaration(entry)) {
            return this.tryParseChunkEntryMethodDecl(entry);
        }
        return undefined;
    }

    @Cache()
    public getDefinedModules(): Record<string, string> | undefined {
        return this
            .getModuleObject()
            ?.properties
            .map(this.tryParseChunkEntry.bind(this))
            .filter(nonNull)
            .reduce(fromEntries<string, string>, {});
    }
}

