import type { WildcardMeta } from "./magic/match/merge-metas";
import type { PreprocessSelector } from "./magic/match/preprocess";
import type { MatchIt } from "./magic/matcher";
import type { NodeTypes } from "./magic/nodes";
import type { Match, Parse } from "./magic";

type parsed = Parse<"CallExpression > Identifier">;

type matched = Match<parsed, NodeTypes>;
//    ^?

type AST = NodeTypes;

type T = parsed;

type inc_left = MatchIt<T["left"], AST>;

type inc_right = PreprocessSelector<
    T["right"],
    WildcardMeta,
    AST
>;
