namespace Lisp

module SymbolTable =

    open Ast
    open Parser
    open Errors

    let nullenv (): Env = ref List.empty

    let keyEq name (k, _) = name = k

    let isBound var (env: Env) = !env |> List.exists (keyEq var)

    let getVar var (env: Env) =
        let result = !env |> List.tryFind (keyEq var)
        match result with
        | None -> throw (UnboundVar("Getting an unbound variable: ", var))
        | Some(_, r) -> !r

    let setVar var value (env: Env) = 
        let result = !env |> List.tryFind (keyEq var)
        match result with
        | Some(_, v) -> v := value ; value
        | None -> throw (UnboundVar("Setting an unbound variable: ", var))
    
    let define (env: Env) var value = 
        let result = !env |> List.tryFind( keyEq var)
        match result with
        | Some(_, v) -> v := value ; value
        | None -> env := [var, ref value] @ !env; value

    let bindVars bindings (env: Env) = 
        ref ((bindings |> List.map (fun (n, v) -> n, ref v)) @ !env)
    

