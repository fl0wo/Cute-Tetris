let rand = System.Random()

module Mate =
    let randDir = (rand.Next(0, 2))

    let minimo (x: int) (y: int) =
        if (x < y) then x
        else y

    let massimo (x: int) (y: int) =
        if (x > y) then x
        else y

    let noOverflow (x: int) (M: int) = massimo 0 (minimo x M)

type Blocco(matrice: array<array<int>>, _rPos: int, _cPos: int) =

    let mutable rPosV: int = _rPos
    let mutable cPosV: int = _cPos

    member this.rPos = rPosV
    member this.cPos = cPosV

    member this.matrice: array<array<int>> = matrice
    member this.righe: int = this.matrice.Length
    member this.colonne: int = this.matrice.[0].Length

    member this.ruotaDx =
        let L = this.matrice.Length

        let array: int [,] = Array2D.init<int> L L (fun i j -> this.matrice.[j].[(L - i - 1)])
        // [| for x in 0..L_R do yield [| for y in 0..L_C do yield (this.matrice.[y].[ (L_C-x-1) ]) |] |];

        for r in 0 .. (L - 1) do
            for c in 0 .. (L - 1) do
                this.matrice.[r].[c] <- array.[r, c]

    member this.gravityRight maxBorder =
        if (cPosV + 1 + this.colonne < maxBorder) then do cPosV <- (this.cPos + 1)

    // controlla collisions
    member this.gravityLeft =
        if (cPosV > 0) then do cPosV <- (this.cPos - 1)

    // controlla collisions
    member this.gravityDown =
        rPosV <- (this.rPos + 1)
        ()

    member this.generateRic n r c (color: int) =
        this.matrice.[r].[c] <- color

        let dir = [| 0; 0 |]
        dir.[(rand.Next 2)] <- 1

        let futureRFixed: int = Mate.noOverflow (r + dir.[0]) (this.righe - 1)
        let futureCFixed: int = Mate.noOverflow (c + dir.[1]) (this.colonne - 1)

        if (n > 0) then this.generateRic (n - 1) futureRFixed futureCFixed color
        else true

    member this.generateRandom (colore: int) =
        let nBlocchi = rand.Next(this.righe, (this.righe * 2))
        let startR = rand.Next(0, (this.righe / 2))
        let startC = rand.Next(0, (this.colonne / 2))

        this.generateRic nBlocchi startR startC colore

    member this.generateQuadrato =
        for r in 0 .. (this.righe - 1) do
            for c in 0 .. (this.colonne - 1) do
                this.matrice.[r].[c] <- 1

module Utils =

    let initBloccoQuadrato L =
        [| for x in 0 .. L do
            yield [| for x in 0 .. L do
                         yield false |] |]

    let initBlocco R C =
        [| for x in 0 .. R do
            yield [| for x in 0 .. C do
                         yield false |] |]

    let initBloccoInt R C colore =
        [| for x in 0 .. R do
            yield [| for x in 0 .. C do
                         yield colore |] |]

type Mappa(r: int, c: int) =
    let _mappa: array<array<int>> = Utils.initBloccoInt r c 0

    member this.mappa = _mappa

    member this.r = r
    member this.c = c

    member this.apply (blocco: Blocco) =
        let L = (blocco.matrice.Length - 1) // to fix
        let rB = blocco.rPos
        let rC = blocco.cPos
        for r in 0 .. L do
            for c in 0 .. L do
                if (blocco.matrice.[r].[c] > 0) then do this.mappa.[rB + r].[rC + c] <- blocco.matrice.[r].[c]


    member this.initFloor =
        // Init floor
        for c in 0 .. (this.c - 1) do
            this.mappa.[(this.r - 1)].[c] <- 1
            this.mappa.[(this.r - 2)].[0] <- 1
            this.mappa.[(this.r - 2)].[(this.c - 1)] <- 1

    member this.getIstanceWith (blocco: Blocco): array<array<int>> =
        let clone: array<array<int>> = Utils.initBloccoInt (this.r) (this.c) 0

        // Apply all falled blocks
        for x in 0 .. (this.r - 1) do
            for y in 0 .. (this.c - 1) do
                clone.[x].[y] <- this.mappa.[x].[y]

        // Apply current block
        let L = (blocco.matrice.Length - 1) // to fix
        let rB = blocco.rPos
        let rC = blocco.cPos
        for r in 0 .. L do
            for c in 0 .. L do
                if (blocco.matrice.[r].[c] > 0) then do clone.[rB + r].[rC + c] <- blocco.matrice.[r].[c]
        clone

    member this.canGoDownAnymore (blocco: Blocco): bool =
        let L = (blocco.matrice.Length - 1) // to fix
        let rB = blocco.rPos
        let rC = blocco.cPos

        let mutable ris: bool = true

        for r in 0 .. L do
            for c in 0 .. L do
                ris <- (ris && (((this.mappa.[(rB + r + 1)].[(rC + c)] > 0) && blocco.matrice.[r].[c] > 0) = false))
        ris

    member this.fastFallDown (blocco: Blocco) =
        while ((this.canGoDownAnymore blocco) = true) do
            blocco.gravityDown
        this.apply blocco

module UtilsView =

    let mutable canPrint = true;

    let rettangolo = '█'
    let quadretto = '■'

    let reset = "\u001b[0m"
    let normal = "\u001b[37;1m"

    let colori: array<string> =
        [| for x in 1 .. 8 do
            yield ("\u001b[3" + x.ToString() + ";1m") |]

    let setWindowSize (w: int) (h: int) = System.Console.SetWindowSize(w, h)

    let cls = printfn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";

    let printBlocco (b: Blocco) = printfn "%A" (b.matrice)

    let printMappa (m: array<array<int>>) =
        if(canPrint = true) then
            canPrint <- false;
            for r in m do
                for c in r do
                    if (c > 1) then printf "%s%c%c" colori.[c] rettangolo rettangolo
                    else if (c = 1) then printf "%s%c%c" normal rettangolo rettangolo
                    else printf "%s  " reset
                printf "\n"
            canPrint <- true;
        else ();

let mutable blocco: Blocco = new Blocco((Utils.initBloccoInt 3 3 0), 3, 6)

blocco.generateRandom (rand.Next(1, 5))

let mappa: Mappa = new Mappa(20, 20)

mappa.initFloor

open System

module Control =

    let onKey (k: string): bool =
        match k with
        | "LeftArrow" ->
            (blocco.gravityLeft
             true)
        | "RightArrow" ->
            (blocco.gravityRight mappa.c
             true)
        | "Spacebar" ->
            (blocco.ruotaDx
             true)
        | "DownArrow" ->
            ( 
            mappa.fastFallDown blocco;
            UtilsView.cls
            UtilsView.printMappa (mappa.getIstanceWith blocco);

            blocco <- new Blocco((Utils.initBloccoInt 3 3 0), 0, 3);
            blocco.generateRandom (rand.Next(1, 5)) |> ignore;
            true)
        | _ -> (false)

let rec reactiveKey() =
    async {
        let! key = Async.FromContinuations(fun (cont, _, _) ->
                       cont (Console.ReadKey())
                       reactiveKey())
        let keyName: string = key.Key.ToString()

        let needToRefresh = Control.onKey keyName
        if needToRefresh then
            UtilsView.cls
            UtilsView.printMappa (mappa.getIstanceWith blocco)
    }
    |> Async.Start

let rec gravity() =
    async {
        do! Async.Sleep(500)

        if (mappa.canGoDownAnymore blocco = true) then
            blocco.gravityDown
            UtilsView.cls
            UtilsView.printMappa (mappa.getIstanceWith blocco)


        if (mappa.canGoDownAnymore blocco = false) then
            mappa.apply blocco
            blocco <- new Blocco((Utils.initBloccoInt 3 3 0), 0, 3)
            blocco.generateRandom (rand.Next(1, 5)) |> ignore

        gravity()
        () // assurdo questo ahaha
    }
    |> Async.Start


reactiveKey()
gravity()

UtilsView.printMappa (mappa.getIstanceWith blocco)

// NO END RN
System.Threading.Thread.Sleep(-1);
