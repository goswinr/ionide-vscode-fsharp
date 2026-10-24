namespace Ionide.VSCode.FSharp

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.VSCode
open Fable.Import.VSCode.Vscode

module CodeLensHelpers =

    // This data structure provided by FSAC, in CodeLensResolve:
    // https://github.com/ionide/FsAutoComplete/blob/258fbc55053f585ea6ebd81a7dc10e4f3573aff6/src/FsAutoComplete/LspServers/AdaptiveFSharpLspServer.fs#L1637

    type LspUri = string

    type LspPosition = { line: uint32; character: uint32 }

    type LspRange =
        { start: LspPosition
          ``end``: LspPosition }

    type LspLocation = { uri: string; range: LspRange }

    type CustomIExports =
        abstract registerCommand:
            command: string * callback: (LspUri -> LspPosition -> LspLocation seq -> obj option) * ?thisArg: obj ->
                Disposable

    let showReferences (uri: LspUri) (args2: LspPosition) (args3: LspLocation seq) =
        let uri = vscode.Uri.parse !!uri
        let pos = vscode.Position.Create(float args2.line, float args2.character)

        let locs =
            args3
            |> Seq.map (fun f ->
                let uri = vscode.Uri.parse f.uri

                let range =
                    vscode.Range.Create(
                        float f.range.start.line,
                        float f.range.start.character,
                        float f.range.``end``.line,
                        float f.range.``end``.character
                    )

                vscode.Location.Create(uri, !^range))
            |> ResizeArray

        commands.executeCommand ("editor.action.showReferences", Some(box uri), Some(box pos), Some(box locs))

    /// Shows the reference counts as inlay hints (e.g. "3 refs") at the end of the declaration line,
    /// in front of the LineLens signature, when `FSharp.codeLenses.references.position` is `inline`.
    module InlineReferences =
        open LanguageService.Types

        let private shortTitle (title: string) =
            // FSAC titles look like "3 References"
            match System.Int32.TryParse(title.Split(' ').[0]) with
            | true, 1 -> "1 ref"
            | true, n -> $"%d{n} refs"
            | _ -> title

        let private toInlayHint (document: TextDocument) (lens: LspCodeLens) =
            match lens.command with
            | Some cmd when
                not (System.String.IsNullOrEmpty cmd.title)
                && lens.range.start.line < document.lineCount
                ->
                let part = vscode.InlayHintLabelPart.Create(shortTitle cmd.title)
                part.tooltip <- Some(U2.Case1 cmd.title)

                // "0 References" has no command
                if not (System.String.IsNullOrEmpty cmd.command) then
                    let command = createEmpty<Command>
                    command.title <- cmd.title
                    command.command <- cmd.command
                    command.arguments <- cmd.arguments |> Option.map (Array.map Some >> ResizeArray)
                    part.command <- Some command

                let line = document.lineAt lens.range.start.line

                let hint =
                    vscode.InlayHint.Create(line.range.``end``, U2.Case2(ResizeArray [ part ]))

                hint.paddingLeft <- Some true
                Some hint
            | _ -> None

        let private provideReferenceHints (document: TextDocument) (range: Range) (token: CancellationToken) =
            promise {
                if not (LanguageService.referenceCodeLensesInline ()) then
                    return ResizeArray()
                else
                    let! lenses = LanguageService.codeLenses document.uri token

                    // like CodeLenses, only resolve the visible ones
                    let! resolved =
                        lenses
                        |> Array.filter (fun lens ->
                            LanguageService.isReferenceCodeLens lens.data
                            && lens.range.start.line >= range.start.line
                            && lens.range.start.line <= range.``end``.line)
                        |> Array.map (fun lens ->
                            LanguageService.resolveCodeLens lens token |> Promise.catch (fun _ -> None))
                        |> Promise.all

                    return
                        resolved
                        |> Array.choose id
                        |> Array.choose (toInlayHint document)
                        |> ResizeArray
            }
            |> Promise.catch (fun _ -> ResizeArray())

        let activate (context: ExtensionContext) =
            let hintsChanged = vscode.EventEmitter.Create<unit>()

            let provider =
                { new InlayHintsProvider with
                    member _.onDidChangeInlayHints
                        with get (): Event<unit> option = Some hintsChanged.event
                        and set (v: Event<unit> option): unit = ()

                    member _.provideInlayHints
                        (
                            document: TextDocument,
                            range: Range,
                            token: CancellationToken
                        ) : ProviderResult<ResizeArray<InlayHint>> =
                        provideReferenceHints document range token |> unbox

                    member _.resolveInlayHint(hint: InlayHint, token: CancellationToken) : ProviderResult<InlayHint> =
                        hint |> unbox }

            languages.registerInlayHintsProvider (LanguageService.selector, provider)
            |> context.Subscribe

            workspace.onDidChangeConfiguration.Invoke(fun e ->
                if e.affectsConfiguration ("FSharp.codeLenses.references.position") then
                    hintsChanged.fire ()
                    LanguageService.refreshCodeLenses ()

                None)
            |> context.Subscribe

            // FSAC refreshes its CodeLenses on save, to pick up references from other files
            workspace.onDidSaveTextDocument.Invoke(fun document ->
                if document.languageId = "fsharp" && LanguageService.referenceCodeLensesInline () then
                    hintsChanged.fire ()

                None)
            |> context.Subscribe

    let activate (context: ExtensionContext) =
        (unbox<CustomIExports> commands)
            .registerCommand (
                "fsharp.showReferences",
                unbox<(LspUri -> LspPosition -> LspLocation seq -> obj option)> (showReferences)
            )
        |> context.Subscribe

        InlineReferences.activate context
