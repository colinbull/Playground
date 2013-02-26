namespace SimpleLib

type Greeter(name :string) = 
    member this.Say(word:string) = sprintf "%s says: %s" name word
