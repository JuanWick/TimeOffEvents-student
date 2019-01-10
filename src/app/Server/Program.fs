module ServerCode.App

open TimeOff
open EventStorage
open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open TimeOff.DomainTypes
open TimeOff.Handlers

// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, RequestEvent>) =

    choose [
        subRoute "/api"
            (choose [
                route "/users/login" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun user ->
                        choose [
                            POST >=> route "/request" >=> requestTimeOffHandler (eventStore) (user)
                            POST >=> route "/validate-request" >=> validateRequestHandler (eventStore) (user)
                            POST >=> routef "/refuse-request/%i/%O" (refuseRequestHandler (eventStore) (user))
                            POST >=> routef "/cancel-request/%i/%O" (employeeCancelRequestHandler (eventStore) (user))                           
                            POST >=> routef "/ask-cancel-request/%i/%O" (askCancelRequestHandler (eventStore) (user))                                        
                            POST >=> routef "/refuse-cancel-request/%i/%O" (refuseCanceledRequestHandler (eventStore) (user))
                            POST >=> routef "/manager-cancel-request/%i/%O" (managerCancelRequestHandler (eventStore) (user))

                            GET >=> routef "/requests/%i/%O" (requestTimeOffByIdHandler (eventStore) (user))
                            GET >=> routef "/requests/%i" (requestTimeOffListByUserIdHandler (eventStore) (user))
                            GET >=> routef "/requests/summary/%i" (requestTimeOffSummaryByUserIdHandler (eventStore) (user))
                            GET >=> routef "/requests/history/%i" (requestTimeOffHistoryByUserIdHandler (eventStore) (user))

                        ]
                    ))
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, RequestEvent>) (app: IApplicationBuilder) =
    printfn "configureApp"

    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "../../../.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, RequestEvent>(storagePath, sprintf "%d")

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0