/* eslint-disable unused-imports/no-unused-vars */

import type { Dnf } from "./magic/match/dnf";
import type { WildcardMeta } from "./magic/match/merge-metas";
import type { CollapseChildRelations, CollapseNegativesFromConjunction, CollapsePositivesFromConjunction, PrecollapseCollectChildBoundaries, PreprocessSelector, SplitConjunction } from "./magic/match/preprocess";
import type { PickNode, PreprocessExtract, TryToNarrowByExtracting } from "./magic/match/utils";
import type { MatchIt } from "./magic/matcher";
import type { NodeTypes, UnmapNode } from "./magic/nodes";
import type { Simplify } from "./magic/utils";
import type { Match, Parse } from "./magic";
import type { ComputedPropertyName } from "typescript";

type guhParse = Parse<"ClassDeclaration > MethodDeclaration[name.text=foo]">;

type guh = Match<guhParse, NodeTypes> & { name: { text: string; }; };

type test_1 = guh["name"];

const test_2: test_1 = null as ComputedPropertyName;

let res: UnmapNode<guh>["name"];

type AST = NodeTypes;


{
    type Narrower<T, U> = T extends U ? T : U extends T ? U : never;

    type T = guhParse;

    type col_left = MatchIt<T["left"], AST>;

    type col_right_arg = PreprocessSelector<T["right"], WildcardMeta, AST>;

    type col_right = Dnf<
        col_right_arg
    > extends { args: infer Res extends any[]; }
        ? Res : never;

    type split_arg_2 = SplitConjunction<col_right[0]["args"]>;

    type split_extract_arg_1 = CollapsePositivesFromConjunction<col_left, split_arg_2["and"], AST>;

    {
        type Left = col_left;

        type Positives = split_arg_2["and"];

        type Acc = PrecollapseCollectChildBoundaries<Left, { field: null; }, AST>;

        {
            type First = Positives[0];

            type extract_3_arg_1 = PrecollapseCollectChildBoundaries<
                Left,
                { field: First["field"]; },
                AST
            >;

            type extract_3_arg_2
                = unknown extends First["extract"]
                    ? Exclude<
                        TryToNarrowByExtracting<
                            First["identifier"] extends null
                                ? AST
                                : PickNode<First["identifier"], AST>,
                            First["extract"]
                        >,
                        First["exclude"]
                    >
                    : Exclude<
                        Exclude<
                            TryToNarrowByExtracting<
                                First["identifier"] extends null
                                    ? AST
                                    : PickNode<First["identifier"], AST>,
                                PreprocessExtract<First["extract"]>["extract"]
                            >,
                            First["exclude"]
                        >,
                        PreprocessExtract<First["extract"]>["exclude"]
                    >;

            type test_1 = extract_3_arg_2 extends extract_3_arg_1 ? true : false;

            const test_2: extract_3_arg_2 = null as any;
            const test_3: extract_3_arg_1 = test_2;

            type test_4 = Simplify<extract_3_arg_1>;

            type test_5 = extract_3_arg_2;

            type test_6 = Extract<test_4, test_5>;

            type extract_2_arg_1 = Narrower<
                extract_3_arg_1,
                extract_3_arg_2
            >;

            type extract_2_arg_2 = First["inferredNodes"] extends null ? any : First["inferredNodes"];


            type extract_1_arg_2 = Extract<
                extract_2_arg_1,
                extract_2_arg_2
            >;

            type result
                = Extract<
                    Acc,
                    extract_1_arg_2
                >;
        }

        type result = CollapsePositivesFromConjunction<Left, Positives, AST, Acc>;
    }

    type split_extract_arg_2 = CollapseNegativesFromConjunction<col_left, split_arg_2["not"], AST>;

    type split = Extract<
        split_extract_arg_1,
        split_extract_arg_2
    >;

    type pre_1 = CollapseChildRelations<
        col_left,
        col_right,
        AST
    >;
}

type pre_1 = PreprocessSelector<guhParse, WildcardMeta, NodeTypes>;
