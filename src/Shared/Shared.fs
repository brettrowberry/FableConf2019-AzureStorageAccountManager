namespace Shared

type FableStorageAccount = {
    Id : string
    Name : string
    Region : string
    Tags : (string * string) []
}

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IStorageApi = {
    list : unit -> Async<FableStorageAccount []>
    create: string -> Async<unit>
    delete: string[] -> Async<unit>
}