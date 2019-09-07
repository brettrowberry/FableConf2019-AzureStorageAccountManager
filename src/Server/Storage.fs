module Storage

open Shared

open Microsoft.Azure.Management.Fluent
open Microsoft.Azure.Management.Storage.Fluent
open Microsoft.Azure.Management.Storage.Fluent.Models
open Microsoft.Azure.Management.ResourceManager.Fluent
open Microsoft.Azure.Management.ResourceManager.Fluent.Core
open System
open System.Collections.Generic

let private azure =
    let azureStorage =
        let subscription = "06279c1b-6b5a-4089-8b9a-754f69a378fb"
        let creds = SdkContext.AzureCredentialsFactory.FromServicePrincipal(
                        clientId = "523fb00f-ee33-495b-b433-6e2240abf5c4",
                        clientSecret = "g:y/Vwu2QaWjYZ38*7RK_biSSw8QR.py",
                        tenantId = "b41bb662-23d3-4774-bf22-934d7cf1b337",
                        environment = AzureEnvironment.AzureGlobalCloud)
        Azure.Configure().Authenticate(creds).WithSubscription(subscription)
    azureStorage.StorageAccounts

let listStorageAccounts () : FableStorageAccount [] =
    let storageAccounts =
            azure.List()
            |> Seq.sortBy (fun s -> s.CreationTime)
            |> Seq.toArray
    let fableStorageAccounts =
        storageAccounts
        |> Array.map (fun sa -> {
           Id = sa.Id
           Name = sa.Name
           Region = sa.RegionName
           Tags = sa.Tags
                |> Seq.map (fun t -> (t.Key, t.Value))
                |> Seq.toArray })
    fableStorageAccounts

let createStorageAccount nickname =
    let storageAccountName = Guid.NewGuid().ToString("N").Substring(0,24)
    let tags = Dictionary<string, string>(dict [ ("nickname", nickname) ])
    let sa = azure.Define(storageAccountName)
               .WithRegion(Region.EuropeWest)
               .WithNewResourceGroup("sam-westeurope")
               .WithGeneralPurposeAccountKindV2()
               .WithSku(StorageAccountSkuType.Standard_LRS)
               .WithOnlyHttpsTraffic()
               .WithBlobStorageAccountKind()
               .WithAccessTier(AccessTier.Cool)
               .WithTags(tags)
               .Create()
    ()

let deleteStorageAccounts ids = azure.DeleteByIds ids