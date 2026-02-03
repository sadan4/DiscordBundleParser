import { assert, expect, it } from "vitest";

import type { WebpackMainChunkParser } from "..";

export function mainChunkTests(parser: WebpackMainChunkParser, delay?: number) {
    it("locates __webpack_require__", function () {
        const n = parser.__webpack_require__;

        expect(n
            ?.declarations
            .map(parser.makeRangeFromAstNode.bind(parser)))
            .toMatchSnapshot();
    }, delay);
    it("locates __webpack_modules__", function () {
        const n = parser.__webpack_modules__;

        expect(n
            ?.declarations
            .map(parser.makeRangeFromAstNode.bind(parser)))
            .toMatchSnapshot();
    }, delay);
    it("gets js chunk hashes", function () {
        const hashes = parser.getJsChunkHashes();

        expect(hashes.toSorted()).toMatchSnapshot();
    });
    it("gets all initial module text", function () {
        const moduleMap = parser.getDefinedModules();

        assert(moduleMap);

        const keys = Object.keys(moduleMap);
        const numEntries = parser.getModuleObject()?.properties.length;

        expect(keys.length, "An entry was missed").to.equal(numEntries);
        expect(keys).toMatchSnapshot();
    });
}

export const DELAY = 15_000;
