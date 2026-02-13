import { query } from "@sadan4/tsquery";
import ts from "typescript";
import { describe, expect, test } from "vitest";

import { DeclarationDomain } from "./declarations";
import { UsageDomain } from "./getUsageDomain";
import { AstParser } from "../AstParser";

describe("collectVariableUsage", () => {
    test("conditional type", () => {
        const parser = new AstParser(`
            export type TrueIfZero<T> = T extends 0 ? true : false;
        `);

        const { sourceFile, vars } = parser;
        const [nameIdentifier] = query(sourceFile, "Identifier");

        const [typeParameterIdentifier] = query(
            sourceFile,
            "TypeParameter Identifier",
        );

        const [conditionalTypeReferenceIdentifier] = query(
            sourceFile,
            "ConditionalType TypeReference Identifier",
        );


        expect(vars).toEqual(new Map([
            [
                nameIdentifier,
                {
                    declarations: [nameIdentifier],
                    domain: DeclarationDomain.Type,
                    exported: true,
                    inGlobalScope: false,
                    uses: [],
                },
            ],
            [
                typeParameterIdentifier,
                {
                    declarations: [typeParameterIdentifier],
                    domain: DeclarationDomain.Type,
                    exported: false,
                    inGlobalScope: false,
                    uses: [
                        {
                            domain: DeclarationDomain.Type,
                            location: conditionalTypeReferenceIdentifier,
                        },
                    ],
                },
            ],
        ]));
    });

    test("class declaration and property", () => {
        const parser = new AstParser(`
            class Box {
                value = 1;
            }

            export const { value } = new Box();
        `);

        const { sourceFile, vars } = parser;

        const [classDeclaration] = query<ts.ClassDeclaration>(
            sourceFile,
            "ClassDeclaration",
        );

        const [variableDeclaration] = query(sourceFile, "VariableDeclaration");

        const [variableIdentifier] = query(
            variableDeclaration,
            "ObjectBindingPattern Identifier",
        );

        const [newIdentifier] = query(
            variableDeclaration,
            "NewExpression Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                classDeclaration.name,
                {
                    declarations: [classDeclaration.name],
                    domain: DeclarationDomain.Type | DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: false,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: newIdentifier,
                        },
                    ],
                },
            ],
            [
                variableIdentifier,
                {
                    declarations: [variableIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: true,
                    inGlobalScope: false,
                    uses: [],
                },
            ],
        ]));
    });

    test("class expression and property", () => {
        const { sourceFile, vars } = new AstParser(`
            const Box = class {
                value = 1;
            }

            export const { value } = new Box();
        `);

        const [classNameIdentifier] = query(
            sourceFile,
            "VariableDeclaration Identifier",
        );

        const [usageVariableDeclaration] = query(
            sourceFile,
            "VariableStatement:has(ExportKeyword) VariableDeclaration",
        );

        const [variableIdentifier] = query(
            usageVariableDeclaration,
            "ObjectBindingPattern Identifier",
        );

        const [newIdentifier] = query(
            usageVariableDeclaration,
            "NewExpression Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                classNameIdentifier,
                {
                    declarations: [classNameIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: false,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: newIdentifier,
                        },
                    ],
                },
            ],
            [
                variableIdentifier,
                {
                    declarations: [variableIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: true,
                    inGlobalScope: false,
                    uses: [],
                },
            ],
        ]));
    });

    test("enum declaration and property", () => {
        const { sourceFile, vars } = new AstParser(`
            enum Values { First }
            Values.First;
        `);

        const [enumIdentifier] = query(sourceFile, "EnumDeclaration Identifier");

        const [propertyReference] = query(
            sourceFile,
            "ExpressionStatement Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                enumIdentifier,
                {
                    declarations: [enumIdentifier],
                    domain: DeclarationDomain.Any,
                    exported: false,
                    inGlobalScope: true,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: propertyReference,
                        },
                    ],
                },
            ],
        ]));
    });

    test("function declaration and call", () => {
        const { sourceFile, vars } = new AstParser(`
            function createValue() {
                return 123;
            }

            createValue();
        `);

        const [functionDeclaration] = query(
            sourceFile,
            "FunctionDeclaration Identifier",
        );

        const [callExpressionIdentifier] = query(
            sourceFile,
            "CallExpression Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                functionDeclaration,
                {
                    declarations: [functionDeclaration],
                    domain: DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: true,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: callExpressionIdentifier,
                        },
                    ],
                },
            ],
        ]));
    });

    test("namespace and property", () => {
        const { sourceFile, vars } = new AstParser(`
            namespace Values { export const First = 0 }
            Values.First;
        `);

        const [namespaceIdentifier] = query(
            sourceFile,
            "ModuleDeclaration Identifier",
        );

        const [variableIdentifier] = query(
            sourceFile,
            "VariableDeclaration Identifier",
        );

        const [propertyReference] = query(
            sourceFile,
            "ExpressionStatement Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                namespaceIdentifier,
                {
                    declarations: [namespaceIdentifier],
                    domain: DeclarationDomain.Namespace | DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: true,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: propertyReference,
                        },
                    ],
                },
            ],
            [
                variableIdentifier,
                {
                    declarations: [variableIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: true,
                    inGlobalScope: false,
                    uses: [],
                },
            ],
        ]));
    });

    test("nested Namespace and property", () => {
        const { sourceFile, vars } = new AstParser(`
            namespace Outer {
                export namespace Inner {
                    export const First = 0;
                }
            }

            Outer.Inner.First;
        `);

        const [namespaceIdentifierOuter] = query(
            sourceFile,
            "ModuleDeclaration Identifier",
        );

        const [namespaceIdentifierInner] = query(
            sourceFile,
            "ModuleDeclaration ModuleDeclaration Identifier",
        );

        const [variableIdentifier] = query(
            sourceFile,
            "VariableDeclaration Identifier",
        );

        const [propertyReference] = query(
            sourceFile,
            "ExpressionStatement Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                namespaceIdentifierInner,
                {
                    declarations: [namespaceIdentifierInner],
                    domain: DeclarationDomain.Namespace,
                    exported: true,
                    inGlobalScope: false,
                    uses: [],
                },
            ],
            [
                namespaceIdentifierOuter,
                {
                    declarations: [namespaceIdentifierOuter],
                    domain: DeclarationDomain.Namespace | DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: true,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: propertyReference,
                        },
                    ],
                },
            ],
            [
                variableIdentifier,
                {
                    declarations: [variableIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: true,
                    inGlobalScope: false,
                    uses: [],
                },
            ],
        ]));
    });

    test("variable reference", () => {
        const { sourceFile, vars } = new AstParser(`
            let value = 123;
            value;
        `);

        const [variableIdentifier] = query(
            sourceFile,
            "VariableDeclaration Identifier",
        );

        const [variableReference] = query(
            sourceFile,
            "ExpressionStatement Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                variableIdentifier,
                {
                    declarations: [variableIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: true,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: variableReference,
                        },
                    ],
                },
            ],
        ]));
    });

    test("variable reference in a block", () => {
        const { sourceFile, vars } = new AstParser(`
            let value = 123;
            
            {
                value;
            }
        `);

        const [variableIdentifier] = query(
            sourceFile,
            "VariableDeclaration Identifier",
        );

        const [variableReference] = query(
            sourceFile,
            "ExpressionStatement Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                variableIdentifier,
                {
                    declarations: [variableIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: true,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: variableReference,
                        },
                    ],
                },
            ],
        ]));
    });

    test("variable with a reference inside a class", () => {
        const { sourceFile, vars } = new AstParser(`
            let value = 123;

            class Getter {
                getValue = () => value;
            }
        `);

        const [variableIdentifier] = query(
            sourceFile,
            "VariableDeclaration Identifier",
        );

        const [classDeclaration] = query<ts.ClassDeclaration>(
            sourceFile,
            "ClassDeclaration",
        );

        const [variableReference] = query(
            classDeclaration,
            "ArrowFunction Identifier",
        );

        expect(vars).toEqual(new Map([
            [
                classDeclaration.name,
                {
                    declarations: [classDeclaration.name],
                    domain: DeclarationDomain.Type | DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: true,
                    uses: [],
                },
            ],
            [
                variableIdentifier,
                {
                    declarations: [variableIdentifier],
                    domain: DeclarationDomain.Value,
                    exported: false,
                    inGlobalScope: true,
                    uses: [
                        {
                            domain: UsageDomain.ValueOrNamespace,
                            location: variableReference,
                        },
                    ],
                },
            ],
        ]));
    });
});
