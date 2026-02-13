// Code largely based on https://github.com/ajafff/tsutils
// Original license: https://github.com/ajafff/tsutils/blob/26b195358ec36d59f00333115aa3ffd9611ca78b/LICENSE

import ts from "typescript";

import {
    DeclarationDomain,
    type DeclarationInfo,
    getDeclarationDomain,
} from "./declarations";
import { type Scope, ScopeBoundary, ScopeBoundarySelector } from "./Scope";
import type {
    InternalUsageInfo,
    Usage,
    UsageInfo,
    UsageInfoCallback,
} from "./usage";

export enum ConditionalTypeScopeState {
    Initial,
    Extends,
    TrueType,
    FalseType,
}

abstract class AbstractScope implements Scope {
    protected namespaceScopes: Map<string, NamespaceScope> | undefined
        = undefined;
    protected uses: Usage[] = [];
    protected variables: Map<string, InternalUsageInfo> = new Map();
    #enumScopes: Map<string, EnumScope> | undefined = undefined;

    constructor(protected global: boolean) {}

    addUse(use: Usage): void {
        this.uses.push(use);
    }

    addVariable(
        identifier: string,
        name: ts.PropertyName,
        selector: ScopeBoundarySelector,
        exported: boolean,
        domain: DeclarationDomain,
    ): void {
        const variables = this.getDestinationScope(selector).getVariables();

        const declaration: DeclarationInfo = {
            declaration: name,
            domain,
            exported,
        };

        const variable = variables.get(identifier);

        if (variable === undefined) {
            variables.set(identifier, {
                declarations: [declaration],
                domain,
                uses: [],
            });
        } else {
            variable.domain |= domain;
            variable.declarations.push(declaration);
        }
    }


    createOrReuseEnumScope(name: string, _exported: boolean): EnumScope {
        let scope: EnumScope | undefined;

        if (this.#enumScopes === undefined) {
            this.#enumScopes = new Map();
        } else {
            scope = this.#enumScopes.get(name);
        }

        if (scope === undefined) {
            // eslint-disable-next-line @typescript-eslint/no-use-before-define
            scope = new EnumScope(this);
            this.#enumScopes.set(name, scope);
        }

        return scope;
    } // only relevant for the root scope

    createOrReuseNamespaceScope(
        name: string,
        _exported: boolean,
        ambient: boolean,
        hasExportStatement: boolean,
    ): NamespaceScope {
        let scope: NamespaceScope | undefined;

        if (this.namespaceScopes === undefined) {
            this.namespaceScopes = new Map();
        } else {
            scope = this.namespaceScopes.get(name);
        }

        if (scope === undefined) {
            // eslint-disable-next-line @typescript-eslint/no-use-before-define
            scope = new NamespaceScope(ambient, hasExportStatement, this);
            this.namespaceScopes.set(name, scope);
        } else {
            scope.refresh(ambient, hasExportStatement);
        }

        return scope;
    }

    end(cb: UsageInfoCallback): void {
        if (this.namespaceScopes !== undefined) {
            this.namespaceScopes.forEach((value) => value.finish(cb));
        }

        this.namespaceScopes = this.#enumScopes = undefined;
        this.applyUses();
        this.variables.forEach((variable) => {
            for (const declaration of variable.declarations) {
                const result: UsageInfo = {
                    declarations: [],
                    domain: declaration.domain,
                    exported: declaration.exported,
                    inGlobalScope: this.global,
                    uses: [],
                };

                for (const other of variable.declarations) {
                    if (other.domain & declaration.domain) {
                        result.declarations.push(other.declaration as ts.Identifier);
                    }
                }

                for (const use of variable.uses) {
                    if (use.domain & declaration.domain) {
                        result.uses.push(use);
                    }
                }

                cb(result, declaration.declaration as ts.Identifier, this);
            }
        });
    }

    abstract getDestinationScope(selector: ScopeBoundarySelector): Scope;

    getFunctionScope(): Scope {
        return this;
    }

    getVariables(): Map<string, InternalUsageInfo> {
        return this.variables;
    }


    markExported(_name: ts.Identifier): void {}


    protected addUseToParent(_use: Usage): void {}

    protected applyUse(use: Usage, variables: Map<string, InternalUsageInfo> = this.variables): boolean {
        const variable = variables.get(use.location.text);

        if (variable === undefined || (variable.domain & use.domain) === 0) {
            return false;
        }

        variable.uses.push(use);
        return true;
    }

    protected applyUses(): void {
        for (const use of this.uses) {
            if (!this.applyUse(use)) {
                this.addUseToParent(use);
            }
        }

        this.uses = [];
    }
}

export class NonRootScope extends AbstractScope {
    constructor(
        protected parent: Scope,
        protected boundary: ScopeBoundary,
    ) {
        super(false);
    }

    getDestinationScope(selector: ScopeBoundarySelector): Scope {
        return this.boundary & selector
            ? this
            : this.parent.getDestinationScope(selector);
    }

    protected override addUseToParent(use: Usage): void {
        return this.parent.addUse(use, this);
    }
}

abstract class AbstractNamedExpressionScope<
    InnerScope extends NonRootScope,
> extends NonRootScope {
    protected abstract get innerScope(): InnerScope;

    #domain: DeclarationDomain;
    #name: ts.Identifier;

    constructor(name: ts.Identifier, domain: DeclarationDomain, parent: Scope) {
        super(parent, ScopeBoundary.Function);
        this.#name = name;
        this.#domain = domain;
    }

    override addUse(use: Usage, source?: Scope): void {
        if (source !== this.innerScope) {
            return this.innerScope.addUse(use);
        }

        if (use.domain & this.#domain && use.location.text === this.#name.text) {
            this.uses.push(use);
        } else {
            return this.parent.addUse(use, this);
        }
    }

    override end(cb: UsageInfoCallback): void {
        this.innerScope.end(cb);
        return cb(
            {
                declarations: [this.#name],
                domain: this.#domain,
                exported: false,
                inGlobalScope: false,
                uses: this.uses,
            },
            this.#name,
            this,
        );
    }

    override getDestinationScope(): InnerScope {
        return this.innerScope;
    }

    override getFunctionScope(): InnerScope {
        return this.innerScope;
    }
}

export class BlockScope extends NonRootScope {
    #functionScope: Scope;

    constructor(functionScope: Scope, parent: Scope) {
        super(parent, ScopeBoundary.Block);
        this.#functionScope = functionScope;
    }

    override getFunctionScope(): Scope {
        return this.#functionScope;
    }
}

export class ClassExpressionScope extends AbstractNamedExpressionScope<NonRootScope> {
    protected innerScope: NonRootScope = new NonRootScope(this, ScopeBoundary.Function);

    constructor(name: ts.Identifier, parent: Scope) {
        super(name, DeclarationDomain.Value | DeclarationDomain.Type, parent);
    }
}

export class ConditionalTypeScope extends NonRootScope {
    #state = ConditionalTypeScopeState.Initial;

    constructor(parent: Scope) {
        super(parent, ScopeBoundary.ConditionalType);
    }

    override addUse(use: Usage): void {
        if (this.#state === ConditionalTypeScopeState.TrueType) {
            return void this.uses.push(use);
        }

        return this.parent.addUse(use, this);
    }

    updateState(newState: ConditionalTypeScopeState): void {
        this.#state = newState;
    }
}

export class EnumScope extends NonRootScope {
    constructor(parent: Scope) {
        super(parent, ScopeBoundary.Function);
    }

    override end(): void {
        this.applyUses();
    }
}

export class FunctionScope extends NonRootScope {
    constructor(parent: Scope) {
        super(parent, ScopeBoundary.Function);
    }

    beginBody(): void {
        this.applyUses();
    }
}

export class FunctionExpressionScope extends AbstractNamedExpressionScope<FunctionScope> {
    protected innerScope: FunctionScope = new FunctionScope(this);

    constructor(name: ts.Identifier, parent: Scope) {
        super(name, DeclarationDomain.Value, parent);
    }

    beginBody(): void {
        return this.innerScope.beginBody();
    }
}

export class NamespaceScope extends NonRootScope {
    #ambient: boolean;
    #exports: Set<string> | undefined = undefined;
    #hasExport: boolean;
    #innerScope = new NonRootScope(this, ScopeBoundary.Function);

    constructor(ambient: boolean, hasExport: boolean, parent: Scope) {
        super(parent, ScopeBoundary.Function);
        this.#ambient = ambient;
        this.#hasExport = hasExport;
    }

    override addUse(use: Usage, source?: Scope): void {
        if (source !== this.#innerScope) {
            return this.#innerScope.addUse(use);
        }

        this.uses.push(use);
    }

    override createOrReuseEnumScope(name: string, exported: boolean): EnumScope {
        if (!exported && (!this.#ambient || this.#hasExport)) {
            return this.#innerScope.createOrReuseEnumScope(name, exported);
        }

        return super.createOrReuseEnumScope(name, exported);
    }

    override createOrReuseNamespaceScope(
        name: string,
        exported: boolean,
        ambient: boolean,
        hasExportStatement: boolean,
    ): NamespaceScope {
        if (!exported && (!this.#ambient || this.#hasExport)) {
            return this.#innerScope.createOrReuseNamespaceScope(
                name,
                exported,
                ambient || this.#ambient,
                hasExportStatement,
            );
        }

        return super.createOrReuseNamespaceScope(
            name,
            exported,
            ambient || this.#ambient,
            hasExportStatement,
        );
    }

    override end(cb: UsageInfoCallback): void {
        this.#innerScope.end((variable, key, scope) => {
            if (
                scope !== this.#innerScope
                || (!variable.exported
                  && (!this.#ambient
                    || (this.#exports !== undefined && !this.#exports.has(key.text))))
            ) {
                return cb(variable, key, scope);
            }

            const namespaceVar = this.variables.get(key.text);

            if (namespaceVar === undefined) {
                this.variables.set(key.text, {
                    declarations: variable.declarations.map(mapDeclaration),
                    domain: variable.domain,
                    uses: [...variable.uses],
                });
            } else {
                outer: for (const declaration of variable.declarations) {
                    for (const existing of namespaceVar.declarations) {
                        if (existing.declaration === declaration) {
                            continue outer;
                        }

                        namespaceVar.declarations.push(mapDeclaration(declaration));
                    }
                }

                namespaceVar.domain |= variable.domain;
                for (const use of variable.uses) {
                    if (namespaceVar.uses.includes(use)) {
                        continue;
                    }

                    namespaceVar.uses.push(use);
                }
            }
        });
        this.applyUses();
        this.#innerScope = new NonRootScope(this, ScopeBoundary.Function);
    }

    finish(cb: UsageInfoCallback): void {
        return super.end(cb);
    }

    override getDestinationScope(): Scope {
        return this.#innerScope;
    }

    override markExported(name: ts.Identifier): void {
        if (this.#exports === undefined) {
            this.#exports = new Set();
        }

        this.#exports.add(name.text);
    }

    refresh(ambient: boolean, hasExport: boolean): void {
        this.#ambient = ambient;
        this.#hasExport = hasExport;
    }
}

export class RootScope extends AbstractScope {
    #exportAll: boolean;
    #exports: string[] | undefined = undefined;
    #innerScope = new NonRootScope(this, ScopeBoundary.Function);

    constructor(exportAll: boolean, global: boolean) {
        super(global);
        this.#exportAll = exportAll;
    }

    override addUse(use: Usage, origin?: Scope): void {
        if (origin === this.#innerScope) {
            return super.addUse(use);
        }

        return this.#innerScope.addUse(use);
    }

    override addVariable(
        identifier: string,
        name: ts.PropertyName,
        selector: ScopeBoundarySelector,
        exported: boolean,
        domain: DeclarationDomain,
    ): void {
        if (domain & DeclarationDomain.Import) {
            return super.addVariable(identifier, name, selector, exported, domain);
        }

        return this.#innerScope.addVariable(
            identifier,
            name,
            selector,
            exported,
            domain,
        );
    }

    override end(cb: UsageInfoCallback): void {
        this.#innerScope.end((value, key) => {
            value.exported
                ||= this.#exportAll
                  || (this.#exports !== undefined && this.#exports.includes(key.text));
            value.inGlobalScope = this.global;
            return cb(value, key, this);
        });
        return super.end((value, key, scope) => {
            value.exported
                ||= scope === this
                  && this.#exports !== undefined
                  && this.#exports.includes(key.text);
            return cb(value, key, scope);
        });
    }

    getDestinationScope(): this {
        return this;
    }

    override markExported(id: ts.Identifier): void {
        if (this.#exports === undefined) {
            this.#exports = [id.text];
        } else {
            this.#exports.push(id.text);
        }
    }
}

function mapDeclaration(declaration: ts.Identifier): DeclarationInfo {
    return {
        declaration,
        domain: getDeclarationDomain(declaration)!,
        exported: true,
    };
}
