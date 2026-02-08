import { describe, expect, it } from "vitest";

import { DELAY, mainChunkTests, SKIP_EXPENSIVE_TESTS } from "./util";
import { getFile } from "../__test__/testingUtil";
import { WebpackMainChunkParser } from "..";

describe("format 1", () => {
    const partParser = new WebpackMainChunkParser(getFile("partWeb.js"));

    const fullParser = SKIP_EXPENSIVE_TESTS
        ? null!
        : new WebpackMainChunkParser(getFile("fullWeb.js"));

    describe("partial file", () => {
        const parser = partParser;

        mainChunkTests(parser);
    });

    describe.skipIf(SKIP_EXPENSIVE_TESTS)("full file", () => {
        const parser = fullParser;

        mainChunkTests(parser, DELAY);
        it("finds the build nunmber", () => {
            expect(parser.getBuildNumber()).toMatchInlineSnapshot(`"440786"`);
        });
    });

    describe("fullFile results are the same as partFile results", function () {
        it.skipIf(SKIP_EXPENSIVE_TESTS)("js chunk hashes match", function () {
            const full = fullParser.getJsChunkHashes().toSorted();
            const part = partParser.getJsChunkHashes().toSorted();

            expect(full).to.deep.equal(part);
        });
    });
});
