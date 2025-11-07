import type { MatchIt } from "./matcher";
import type { NodeTypes, UnmapNode } from "./nodes";
import type { ParseIt } from "./parser";

export type Parse<Selector extends string> = ParseIt<Selector>;
export type Match<SelectorAST> = UnmapNode<MatchIt<
    SelectorAST,
    NodeTypes
>>;
export type Query<
    Selector extends string,
> = Match<Parse<Selector>>;
