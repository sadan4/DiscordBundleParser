import { defineConfig } from "vitest/config";
export default defineConfig({
    test: {
        coverage: {
            exclude: ["**/__test__/**", "./coverage/**", "**/dist/**", "./eslint.config.mts"]
        },
        pool: "vmThreads"
    }
})