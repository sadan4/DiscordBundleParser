import { describe, expect, it } from "vitest";

import { getFile } from "./__test__/testingUtil";
import { WebpackLazyChunkParser } from "./WebpackLazyChunkParser";

describe("LazyChunkParser", function () {
    describe("old format", () => {
        it("gets modules from a lazy chunk", function () {
            const parser = new WebpackLazyChunkParser(getFile("lazyChunk.js"));
            const modules = parser.getDefinedModules();

            expect(modules).toMatchSnapshot();
        });
        it("gets modules from an i18n chunk", function () {
            const parser = new WebpackLazyChunkParser(getFile("lazyChunk-i18n.js"));
            const modules = parser.getDefinedModules();

            expect(modules).toMatchSnapshot();
        });
        it("gets chunk id from a lazy chunk", function () {
            const parser = new WebpackLazyChunkParser(getFile("lazyChunk.js"));
            const { chunkId } = parser;

            expect(chunkId).toMatchInlineSnapshot(`"24314"`);
        });
    });
    describe("new format", () => {
        it("gets modules from a lazy chunk", () => {
            const parser = new WebpackLazyChunkParser(getFile("lazyChunk2.js"));
            const modules = parser.getDefinedModules();

            expect(modules).toMatchSnapshot();
        });
        it("gets modules from an i18n chunk", () => {
            const parser = new WebpackLazyChunkParser(getFile("lazyChunk2-i18n.js"));
            const modules = parser.getDefinedModules();

            expect(modules).toMatchSnapshot();
        });
        it("gets chunk id from a lazy chunk", () => {
            const parser = new WebpackLazyChunkParser(getFile("lazyChunk2.js"));
            const { chunkId } = parser;

            expect(chunkId).toMatchInlineSnapshot(`"52694"`);
        });
    });
});
