namespace ProjectEuler

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Linq;
open System.Numerics

type public MathLib() =
    member this.Multiply lst = 
        lst |> Seq.fold (fun acc elem -> acc * elem) 1

    member this.Factors (number: int) = seq {
                for divisor in 1 .. (float >> sqrt >> int) number do
                if number % divisor = 0 then
                    yield divisor
                    yield number / divisor
                }

    member this.Factors (number: int64) = seq {
                for divisor in 1L .. (float >> sqrt >> int64) number do
                if number % divisor = 0L then
                    yield divisor
                    yield number / divisor
                }


    member this.IsPrime x =
        let rec check i =
            double i > sqrt (double x) || (x % i <> 0L && check(i + 1L))
        check 2L

    member this.Factorial(number:BigInteger) = 
        let rec factorial n:BigInteger =
            if n=0I then 1I else n * factorial(n - 1I) 

        factorial number

type public PrimesGenerator() =
    
    member this.getPrimes2 max = 
        let primes = new BitArray(max+1, true)
        seq { 2 .. max } |>
        Seq.filter (fun n -> 
        if primes.[n] then
            for i in int64 n * int64 n..int64 n..int64 max do primes.[int i] <- false
        primes.[n])
    
    member this.getPrimes3 max = 
        let primes = new BitArray(max+1, true)
        [ for n in 2..max do
                if primes.[int n] then
                    for i in int64 n * int64 n..int64 n..int64 max do primes.[int i] <- false
                    yield n ]
    
    member this.getPrimesMax max = 
        let primes = new BitArray(max+1, true)
        let result = new ResizeArray<int>(max/10)
        for n = 2 to max do
            if primes.[n] then
                let start = (int64 n * int64 n)
                if start < int64 max then
                    let i = ref (int start)
                    while !i <= max do primes.[!i] <- false; i := !i + n
                result.Add n
        result

    member this.prime_series_sieve (limit:bigint) = 
        let series = List.toArray [0I..limit]
        series.SetValue(0I, 1)
 
        let rec eliminate_multiples (n:bigint) (i:bigint) = 
            let index = (i * n)
            if index < bigint.Parse(series.Length.ToString()) then 
                series.SetValue(0I, (int64)index)
                eliminate_multiples n (i + 1I)
 
        for n in [2I..(limit/2I)] do
            eliminate_multiples n 2I
 
        series


    member this.PrimesInfinite () = 
        let rec nextPrime n p primes =
            if primes |> Map.containsKey n then
                nextPrime (n + p) p primes
            else
                primes.Add(n, p)

        let rec prime n primes =
            seq {
                if primes |> Map.containsKey n then
                    let p = primes.Item n
                    yield! prime (n + 1) (nextPrime (n + p) p (primes.Remove n))
                else
                    yield n
                    yield! prime (n + 1) (primes.Add(n * n, n))
            }

        prime 2 Map.empty



type public Problems() = 

    member this.Problem1() =
        [1..999] |> Seq.filter (fun i -> i % 3 = 0 || i % 5 = 0) |> Seq.sum

    member this.Problem2() =
        Seq.unfold (fun state ->
            if (snd state > 4000000) then None
            else Some(fst state + snd state, (snd state, fst state + snd state))) (1,1)
            |> Seq.filter (fun x -> x % 2 = 0) |> Seq.sum

    member this.Problem3() =
        let mathLib = new MathLib();
        mathLib.Factors 600851475143L |> Seq.filter(fun i -> mathLib.IsPrime i) |> Seq.max

    member this.Problem4() = 
        let reverseNumber value = Convert.ToInt32(new string(Array.rev (value.ToString().ToCharArray())))

        [for i in [100..999] do
            for j in [100..999] do yield i * j]
                |> Seq.filter (fun x -> x = reverseNumber(x)) |> Seq.max

    member this.Problem5() = 
        let values = [1..20]
        let reverseValues = values |> List.rev |> Seq.toList
        let max = values |> Seq.max
        let nums = max |> Seq.unfold (fun i -> Some (i, i + max))
        let divisibleByList list n =
            list |> Seq.forall (fun x -> n % x = 0) 

        nums |> Seq.find (fun i -> divisibleByList reverseValues i)


    member this.Problem6() = 
        let values = [1..100]
        let sumOfSquares = values |> Seq.map (fun x -> pown x 2) |> Seq.sum
        let squareOfSums = values |> Seq.sum |> (fun x -> pown x 2)

        squareOfSums - sumOfSquares


    member this.Problem7() =
        let primes = new PrimesGenerator()
        primes.PrimesInfinite() |> Seq.skip 10000 |> Seq.take 1 |> Seq.head

    member this.Problem8() = 
        let mathLib = new MathLib()
        let s = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
        let values = s.ToCharArray() |> Seq.map (fun x -> Int32.Parse(x.ToString())) |> Seq.toList
        [0..(values.Length - 5)] 
            |> Seq.map (fun h -> [0..4] |> Seq.map (fun a -> values.[(h + a)]) |> mathLib.Multiply  )
            |> Seq.max

    member this.Problem9() = 
        let pythagorean_triplets(m:int, n:int) =
            let a = m*m-n*n
            let b = 2*m*n
            let c = m*m+n*n
            [a;b;c]
        let tops = 1000
        [for m in [1..tops] do
            for n in [1..m-1] do yield (m, n)] |> Seq.map (fun t -> pythagorean_triplets((fst t, snd t)))
                |> Seq.filter (fun x -> x |> Seq.sum = tops) |> Seq.head |> Seq.fold (fun acc elem -> acc * elem) 1


    member this.Problem10() = 
        let primesGen = new PrimesGenerator()
        primesGen.getPrimesMax 2000000 |> Seq.map (fun prime -> (int64)prime) |> Seq.sum

    member this.Problem11() = 
        let text =
            @"08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
            49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
            81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
            52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
            22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
            24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
            32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
            67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
            24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
            21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
            78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
            16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
            86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
            19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
            04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
            88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
            04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
            20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
            20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
            01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"

        let matrix = text.Split([|'\n'|]) |> Seq.map (fun s -> s.Trim().Split([|' '|]) |> Seq.map (fun c -> Int32.Parse(c.ToString().Trim())) |> Seq.toList) |> Seq.toList
        let product =  Seq.fold(fun acc a -> acc * a) 1

        let ary = Array2D.init matrix.Length matrix.Head.Length  (fun x y -> matrix.[x].[y])
//        let upDown(x:int, y:int) =  if(y + 3 < Array2D.length1 ary - 1) then 
//                                        [0..3] |> Seq.map (fun b -> ary.[x, (y + b)]) |> Seq.sum
//                                    else 0
//        let leftRight(x:int, y:int) =   if(x + 4 < Array2D.length2 ary) then 
//                                            [0..3] |> Seq.map (fun b -> ary.[x + b, y]) |> product
//                                        else 0
//
//        let diagnonalDownRight(x:int, y:int) =  if(x + 4 < (Array2D.length1 ary - 1) && y + 4 < (Array2D.length1 ary - 1)) then 
//                                                    [0..3] |> Seq.map (fun b -> ary.[x + b, y + b]) |> product
//                                                else 0

        let diagnonalDownLeft(x:int, y:int) =   if(x - 4 > 0 && x <= Array2D.length1 ary - 1 && y >= 0 && y <= Array2D.length1 ary - 4) then 
                                                    [0..3] |>     
                                                    Seq.map (fun b -> ary.[x - b, y + b]) |> product
                                                else 0

        let arrayBounds = Array2D.length1 ary - 1
        let h = [for i in [0..arrayBounds] do
                        for j in [0..arrayBounds] do 
        //                    yield upDown(i, j)
        //                    yield leftRight(i, j)
        //                    yield diagnonalDownRight(i, j)
                            yield diagnonalDownLeft(i, j)
                            ] |> Seq.max
        5


    member this.Problem12() = 
        let math = new MathLib()
        let triangles = Seq.unfold (fun state -> Some(fst state + snd state, (fst state + 1, fst state + snd state))) (1, 0)
        triangles |> Seq.map (fun x -> (x, math.Factors(x) |> Seq.length)) |> Seq.filter (fun y -> (snd y) > 500) |> Seq.head |> fst

    static member Problem13() = 
        let text =  
            @"37107287533902102798797998220837590246510135740250
            46376937677490009712648124896970078050417018260538
            74324986199524741059474233309513058123726617309629
            91942213363574161572522430563301811072406154908250
            23067588207539346171171980310421047513778063246676
            89261670696623633820136378418383684178734361726757
            28112879812849979408065481931592621691275889832738
            44274228917432520321923589422876796487670272189318
            47451445736001306439091167216856844588711603153276
            70386486105843025439939619828917593665686757934951
            62176457141856560629502157223196586755079324193331
            64906352462741904929101432445813822663347944758178
            92575867718337217661963751590579239728245598838407
            58203565325359399008402633568948830189458628227828
            80181199384826282014278194139940567587151170094390
            35398664372827112653829987240784473053190104293586
            86515506006295864861532075273371959191420517255829
            71693888707715466499115593487603532921714970056938
            54370070576826684624621495650076471787294438377604
            53282654108756828443191190634694037855217779295145
            36123272525000296071075082563815656710885258350721
            45876576172410976447339110607218265236877223636045
            17423706905851860660448207621209813287860733969412
            81142660418086830619328460811191061556940512689692
            51934325451728388641918047049293215058642563049483
            62467221648435076201727918039944693004732956340691
            15732444386908125794514089057706229429197107928209
            55037687525678773091862540744969844508330393682126
            18336384825330154686196124348767681297534375946515
            80386287592878490201521685554828717201219257766954
            78182833757993103614740356856449095527097864797581
            16726320100436897842553539920931837441497806860984
            48403098129077791799088218795327364475675590848030
            87086987551392711854517078544161852424320693150332
            59959406895756536782107074926966537676326235447210
            69793950679652694742597709739166693763042633987085
            41052684708299085211399427365734116182760315001271
            65378607361501080857009149939512557028198746004375
            35829035317434717326932123578154982629742552737307
            94953759765105305946966067683156574377167401875275
            88902802571733229619176668713819931811048770190271
            25267680276078003013678680992525463401061632866526
            36270218540497705585629946580636237993140746255962
            24074486908231174977792365466257246923322810917141
            91430288197103288597806669760892938638285025333403
            34413065578016127815921815005561868836468420090470
            23053081172816430487623791969842487255036638784583
            11487696932154902810424020138335124462181441773470
            63783299490636259666498587618221225225512486764533
            67720186971698544312419572409913959008952310058822
            95548255300263520781532296796249481641953868218774
            76085327132285723110424803456124867697064507995236
            37774242535411291684276865538926205024910326572967
            23701913275725675285653248258265463092207058596522
            29798860272258331913126375147341994889534765745501
            18495701454879288984856827726077713721403798879715
            38298203783031473527721580348144513491373226651381
            34829543829199918180278916522431027392251122869539
            40957953066405232632538044100059654939159879593635
            29746152185502371307642255121183693803580388584903
            41698116222072977186158236678424689157993532961922
            62467957194401269043877107275048102390895523597457
            23189706772547915061505504953922979530901129967519
            86188088225875314529584099251203829009407770775672
            11306739708304724483816533873502340845647058077308
            82959174767140363198008187129011875491310547126581
            97623331044818386269515456334926366572897563400500
            42846280183517070527831839425882145521227251250327
            55121603546981200581762165212827652751691296897789
            32238195734329339946437501907836945765883352399886
            75506164965184775180738168837861091527357929701337
            62177842752192623401942399639168044983993173312731
            32924185707147349566916674687634660915035914677504
            99518671430235219628894890102423325116913619626622
            73267460800591547471830798392868535206946944540724
            76841822524674417161514036427982273348055556214818
            97142617910342598647204516893989422179826088076852
            87783646182799346313767754307809363333018982642090
            10848802521674670883215120185883543223812876952786
            71329612474782464538636993009049310363619763878039
            62184073572399794223406235393808339651327408011116
            66627891981488087797941876876144230030984490851411
            60661826293682836764744779239180335110989069790714
            85786944089552990653640447425576083659976645795096
            66024396409905389607120198219976047599490197230297
            64913982680032973156037120041377903785566085089252
            16730939319872750275468906903707539413042652315011
            94809377245048795150954100921645863754710598436791
            78639167021187492431995700641917969777599028300699
            15368713711936614952811305876380278410754449733078
            40789923115535562561142322423255033685442488917353
            44889911501440648020369068063960672322193204149535
            41503128880339536053299340368006977710650566631954
            81234880673210146739058568557934581403627822703280
            82616570773948327592232845941706525094512325230608
            22918802058777319719839450180888072429661980811197
            77158542502016545090413245809786882778948721859617
            72107838435069186155435662884062257473692284509516
            20849603980134001723930671666823555245252804609722
            53503534226472524250874054075591789781264330331690"
        let matrix = text.Split([|'\n'|]) |> Seq.map (fun s ->  BigInteger.Parse(s.ToString().Trim()))
        matrix |> Seq.sum |> (fun x -> x.ToString().Substring(0, 10))

    member this.Problem14() = 
        let isEven (a:int64) = a % 2L = 0L
        let dict = new Dictionary<int64, int64>()
        dict.Add(1L, 1L)
        let rec getValue b = 
            if not(dict.ContainsKey(b)) then 
                let ev = isEven b
                let n = match ev with
                            | true -> b/2L
                            | _ -> (3L * b) + 1L
                let prev = getValue(n)
                dict.Add(b, prev + 1L)
            dict.[b]
        [500000L..999999L] |> Seq.map (fun x -> (x, getValue(x))) |> Seq.maxBy (fun x -> snd x) |> fst
        

    member this.Problem15() = 
        let mathLib = new MathLib()
        mathLib.Factorial 40I / ((mathLib.Factorial 20I) * (mathLib.Factorial 20I ))
//        let rec routeCalc x y =
//            match (x, y) with
//                        | var1 when fst var1 = 1 && snd var1 = 1 -> 2
//                        | var1 when fst var1 = 0 || snd var1 = 0 -> 1
//                        | _ -> routeCalc (x - 1) y  + routeCalc x (y - 1)
//        routeCalc 20 20



    static member Problem16() = 
        BigInteger.Pow(2I, 1000) |> (fun x -> x.ToString().ToCharArray()) |> Seq.map (fun x -> BigInteger.Parse(x.ToString())) |> Seq.sum |> fun x -> Convert.ToInt64(x.ToString())


    member this.Problem19() = 
        let startDate = new DateTime(1901, 1, 1, 12, 00, 00)
        let endDate = new DateTime(2000, 12, 31, 23, 59, 59)
        Seq.unfold (fun state ->
            Some(state, (state. AddDays((float)7):DateTime)))  (new DateTime(1900, 1, 7)) 
            |> Seq.skipWhile (fun d -> d <= startDate)
            |> Seq.takeWhile (fun d -> d <= endDate)
            |> Seq.filter (fun d -> d.DayOfWeek = DayOfWeek.Sunday && d.Day = 1)
            |> Seq.length

    member this.Problem20() = 
        let math = new MathLib()
        math.Factorial 100I |> (fun x -> x.ToString().ToCharArray()) |> Seq.map (fun x -> BigInteger.Parse(x.ToString())) |> Seq.sum

    member this.Problem21() = 
        let math = new MathLib()
        let factorSumLookup = {0..10000} |> 
                                Seq.map (fun x -> (x, (math.Factors(x) |> Seq.filter (fun f -> f <> x ) |> Seq.sum))) |> 
                                Map.ofSeq
        factorSumLookup |> 
                Seq.filter (fun x -> x.Value <= 10000 && x.Key <> x.Value && x.Key = factorSumLookup.[x.Value] ) |> 
                Seq.map (fun x -> x.Key) |> 
                Seq.distinct |> 
                Seq.sum 

    member this.Problem22() = 
        let names = File.ReadAllText(@"names.txt").Split [|','|] |> Seq.map (fun x -> x.Trim([|'"'|]))
        let stringToIntSeq (name:string) = name.ToString().ToLower().ToCharArray() |> Seq.map (fun x -> ((int)x) - 96)
        names |> Seq.sort |> Seq.mapi (fun index value -> (index + 1) * (stringToIntSeq(value) |> Seq.sum) ) |> Seq.sum

    member this.Problem23() = 
        let math = new MathLib()
        let relaventSet = {1..28123}
        let abundantNumbers = relaventSet |> Seq.filter(fun x -> ((math.Factors(x) |> Seq.filter(fun y -> y <> x) |> Seq.distinct) |> Seq.sum) > x) |> Seq.cache
        let abundantPairs = abundantNumbers |> Seq.collect(fun x -> abundantNumbers |> Seq.map(fun v -> (x, v)))
        let abundantSums = new Set<int>(abundantPairs |> Seq.map(fun g -> fst g + snd g))
        relaventSet |> Seq.filter(fun h -> not(abundantSums.Contains h)) |> Seq.sum

    //http://stackoverflow.com/questions/1526046/f-permutations
    member this.Problem24() = 
        let rec distribute e = function
              | [] -> [[e]]
              | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

        let rec permute = function
              | [] -> [[]]
              | e::xs -> List.collect (distribute e) (permute xs)
        permute([0..9]) |> Seq.map (fun x -> Convert.ToInt64(String.Join("",  x |> Seq.map(fun v -> v.ToString()))))
            |> Seq.sort |> Seq.skip 999999 |> Seq.head

    member this.Problem25() = 
        Seq.unfold (fun state ->
                Some(fst state + snd state, (snd state, fst state + snd state))) (1I,0I) |>
                Seq.mapi (fun index v -> (index + 1, v)) |> 
                Seq.filter (fun x -> (snd x).ToString().Length = 1000) |>
                Seq.head |> fun x -> fst x

//    member this.Problem28() = 
//        let matrixSize = 1001
//        let matrixValues = {1..(pown 1001 2)}
//        let array = Array2D.init 1001 1001 (fun i j -> 0)
//        let tail_euler28 n =
//          let rec spiral n sum =
//            match n with
//            | 1 -> sum + 1
//            | _ -> spiral (n - 2) (sum + (4 * n * n) - (6 * n) + 6)
//          spiral n 0



    member this.Problem29() = 
        [for i in [2I..100I] do
            for j in [2..100] do yield (i, j)] |> Seq.map (fun pair -> BigInteger.Pow((fst pair), (snd pair))) |> Set.ofSeq |> Seq.length


    member this.Problem30() = 
        let digits (value:int) = value.ToString() |> Seq.map (fun x -> Convert.ToInt32(x.ToString()))
        {2..354294} |> Seq.filter (fun x -> (digits(x) |> Seq.map (fun y -> pown y 5) |> Seq.sum) = x) |> Seq.sum

    member this.Problem34() = 
        let math = new MathLib()
        let y(i: BigInteger) = i.ToString().ToCharArray() |> Seq.map(fun x -> BigInteger.Parse(x.ToString())) |> Seq.map(fun h -> math.Factorial(h)) |> Seq.sum
        {3I..999999I} |> Seq.filter(fun f -> y(f) = math.Factorial(f))  |> Seq.sum

    member this.Problem35() = 
        let primeGen = new PrimesGenerator()
        let primes = primeGen.PrimesInfinite() |> Seq.takeWhile(fun x -> x < 1000000)
        let primeSet = new HashSet<int>(primes)
        let circular (i:int) = 
            let s = i.ToString()
            [0..s.Length-1] |> Seq.map(fun x -> s.Substring(x) + s.Substring(0, x) ) |> Seq.map(fun y -> Convert.ToInt32(y))
        primeSet |> Seq.filter(fun prime -> circular(prime) |> Seq.forall(fun cp -> primeSet.Contains(cp))) |> Seq.length


    member this.Problem36() = 
        let reverse (v:string) = v.ToCharArray() |> Array.rev |> (fun x -> new string(x))
        {1..999999} |> Seq.filter (fun x ->  
            let base10 = x.ToString()
            let base2 = Convert.ToString(x, 2)
            base10 = reverse(base10) && base2 = reverse(base2)) |> Seq.sum

    member this.Problem37() = 
        let primeGen = new PrimesGenerator()
        let math = new MathLib();
        let allPrime seq = seq |> Seq.forall (fun (d:int) -> (math.Factors(d) |> Seq.distinct |> Seq.length) = 2 )
        let removeLeft(x:int) = let s = x.ToString()
                                [for y in 0..(s.Length - 1) do yield Convert.ToInt32(s.Substring(y, s.Length - y))]
        let removeRight(x:int) =    let s = x.ToString()
                                    [for y in 1..(s.Length) do yield Convert.ToInt32(s.Substring(0, y))]
        primeGen.PrimesInfinite() |> 
                Seq.skipWhile (fun s -> s < 10) |>
                Seq.filter (fun x -> allPrime(removeLeft(x)) && allPrime(removeRight(x))) |>
                Seq.take 11 |> 
                Seq.sum

    member this.Problem39() = 
        let pythagorean_triplets(m:int, n:int) =
            let a = m*m-n*n
            let b = 2*m*n
            let c = m*m+n*n
            [a;b;c]
        let tops = 1000
//        fst([for m in [1..tops] do
//                    for n in [1..m-1] do yield (m, n)] 
//                        |> Seq.map (fun t -> pythagorean_triplets(fst t, snd t) |> Seq.sum)
//                        |> Seq.filter(fun x -> x <= 1000)
//                        |> Seq.groupBy(fun o -> o)
//                        |> Seq.map(fun u -> (fst u, (snd u) |> Seq.length)) 
//                        |> Seq.maxBy(fun t -> snd t))
        let d = [for m in [1..tops] do
                    for n in [1..m-1] do yield (m, n)] 
                        |> Seq.map (fun t -> pythagorean_triplets(fst t, snd t) |> Seq.sum)
                        |> Seq.filter(fun x -> x <= 1000)
                        |> Seq.groupBy(fun o -> o)
                        |> Seq.map(fun u -> (fst u, (snd u) |> Seq.length)) 
                        |> Seq.toList
        4


    member this.Problem45() = 
        5

    member this.Problem48() = 
        seq { for i in [1..1000] do yield (BigInteger.Pow(new BigInteger(i), i)) } |> Seq.sum |> 
            fun x ->
                let s = x.ToString()
                s.Substring(s.Length - 10)
        
    member this.Problem50() = 
        let primesGen = new PrimesGenerator()
        let mathLib = new MathLib()

        let primesBelow = primesGen.PrimesInfinite() |> Seq.takeWhile(fun x -> x < 1000000) |> Seq.toList
        let primesToCheck = primesBelow |> Set.ofList
        let y = List.scan (fun agg i -> agg + i) 0 (primesBelow )
        y |> Seq.takeWhile(fun x -> x < 1000000)
            |> Seq.filter(fun x -> primesToCheck.Contains x ) |> Seq.max

    member this.Problem52() =
        let inc = Seq.initInfinite (fun i -> i + 1)
        let funcIntSeq i = i.ToString() |> Seq.map(fun j -> Int32.Parse(j.ToString())) |> Seq.sort
        inc |> Seq.filter(fun x -> 
                                    let x1  = funcIntSeq(1 * x)
                                    let x2 = funcIntSeq(2 * x)
                                    let x3 = funcIntSeq(3 * x)
                                    let x4 = funcIntSeq(4 * x)
                                    let x5 = funcIntSeq(5 * x)
                                    let x6 = funcIntSeq(6 * x)
                                    Enumerable.SequenceEqual(x1, x2) && Enumerable.SequenceEqual(x2, x3) && Enumerable.SequenceEqual(x3, x4) && Enumerable.SequenceEqual(x4, x5) && Enumerable.SequenceEqual(x5, x6))
            |> Seq.head








