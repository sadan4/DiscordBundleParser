import { expect, it } from "vitest";

import { describe } from "node:test";

import { DELAY, mainChunkTests } from "./util";
import { getFile } from "../__test__/testingUtil";
import { WebpackMainChunkParser } from "..";

describe("format 2", () => {
    const fullParser = new WebpackMainChunkParser(getFile("fullWeb2.js"));
    const partParser = new WebpackMainChunkParser(getFile("partWeb2.js"));

    describe("partial file", () => {
        const parser = partParser;

        mainChunkTests(parser);
    });

    describe("full file", () => {
        const parser = fullParser;

        mainChunkTests(parser, DELAY);
    });

    describe("fullFile results are the same as partFile results", function () {
        it("js chunk hashes match", function () {
            const full = fullParser.getJsChunkHashes().toSorted();
            const part = partParser.getJsChunkHashes().toSorted();

            expect(full).to.deep.equal(part);
        });
    });
});
