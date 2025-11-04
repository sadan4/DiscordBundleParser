import type { MatchIt } from "./matcher";
import type { TypeKey } from "./nodes";
import type { ParseIt } from "./parser";

export type Parse<Selector extends string> = ParseIt<Selector>;
export type Match<SelectorAST, AST extends { [TypeKey]: string; }> = MatchIt<
    SelectorAST,
    AST
>;
export type Query<
    Selector extends string,
    AST extends { [TypeKey]: string; },
> = Match<Parse<Selector>, AST>;
