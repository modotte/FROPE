open System
open FSharpPlus


// Some exercises to learn ROP/Kleisli composition subset
// Derived from Schott Wlaschin ROP examples

// Override FSharplus functor operator
// TODO: Need to checkout why
let inline (<!>) x f = map f x

let inline (|>!) x f = tap f x



// EX1
// Simple, straightforward downstream function bypassing via standard Error monad
// and its Result bind operator.
type Data = { 
    Name: string
    Email: string 
}

let nameNotBlank data =
    if String.IsNullOrEmpty data.Name |> not then
        Ok data
    else
        Error "Name is blank!"

let emailNotBlank data =
    if String.IsNullOrEmpty data.Email |> not then
        Ok data
    else
        Error "Email is blank!"


let nameNotLessThanNCharacters (n: int) (data: Data) : Result<Data, string> =
    if String.length data.Name < n then
        Ok data
    else
        Error "Name cannot be less than 10 characters!"

let validateRequest1 data =
    data
    >>= nameNotBlank
    >>= nameNotLessThanNCharacters 10
    >>= emailNotBlank

let userData1 = { Name = ""; Email = "kamaki.h4@gmail.com" }

// validateRequest1 (Ok userData1)

// EX2
// Multiple parameter validators, and using different applicatives and functors
// to lift and construct different unlifted types into monadic ones.
// We'll reuse other previous validator functions.

let decoratedName f l (data: Data) =
    { data with Name = $"{f} {data.Name} {l}" }

let doIOStuff data =
    // Simulate exception->error
    // Usually, IO stuff returns unit
    ()

let validateRequest2 (data: Result<Data, string>): Result<Data, string> =
    data
    >>= (nameNotLessThanNCharacters 10)
    >>= (tap doIOStuff >> Ok)
    <!> (decoratedName "Sir" "Jameson")


let userData2 = { Name = "Lancelot"; Email = "lancelot@email.com" }