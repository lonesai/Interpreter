fun printList([] : string list) = []
  | printList(h::t) = ( print(h ^ "\n"); printList(t) )

fun printlol([]) = []
  | printlol(h::t) = let
    fun printlist([] : string list) = []
      | printlist(h::t) = (print(h ^ ","); printlist(t))
  in
    (print("[");printlist(h); print("]" ^ "\n"); printlol(t))
  end

fun printDic([]) = []
  | printDic((k,v)::t) = (print("(" ^ k ^ "," ^ v ^ ")" ^ "\n"); printDic(t))

fun isInt([] : char list) = true
  | isInt(h::t) = let
    fun isDigit(x) = ((ord x > 47) andalso (ord x < 58)) orelse (ord x = 126)
    in
      if isDigit(h) = false then false
      else isInt(t)
    end

fun isBool(str: string) =
  if str = ":true:" orelse str = ":false:" then true
  else
    false

fun inDic ([] , key) = false
  | inDic((k,v)::t, key) =
    if k = key then true else inDic(t,key)

fun infunDic ([], key) = false
  | infunDic((funName,funtoEnd,dic,funPram,isInOut)::t, key) =
    if funName = key then true else infunDic(t, key)

fun getVal([], key) = "getValError"
  | getVal((k,v)::t, key) =
    if k = key then v else getVal(t,key)

fun getfuntoEnd([], key) = ["getfuntoEndError"]
  | getfuntoEnd( (funName,funtoEnd,dic,funPram,isInOut)::t, key) =
    if funName = key then funtoEnd else getfuntoEnd(t,key)

fun getdic([], key) = []
      | getdic((funName,funtoEnd,dic,funPram,isInOut)::t, key) =
        if funName = key then dic else getdic(t,key)

fun getfunPram([], key) = "getfunPramError"
      | getfunPram((funName,funtoEnd,dic,funPram,isInOut)::t, key) =
          if funName = key then funPram else getfunPram(t,key)

fun getisInOut([], key) = false
      | getisInOut((funName,funtoEnd,dic,funPram,isInOut)::t, key) =
          if funName = key then isInOut else getisInOut(t,key)

fun isalnum([] : char list) = true
  | isalnum(h::t) = if (Char.isAlphaNum(h) = false) then false else isalnum(t)

fun stripNewline(xs) =
	case xs of
		[] => raise List.Empty
	  | (x::[]) => []
	  | (x::xs') => x::stripNewline(xs')

fun replaceNeg(input : char list) = if hd input = #"-" then #"~"::tl(input) else input

fun replaceTilde(input : char list) = "-" ^ implode (tl input)

fun replaceQuotes(input : char list) =
  let
    val input = implode input
  in
    String.substring(input,1,String.size(input)-2)
  end

fun reverse(xs) =
    case xs of
      [] => []
      | (x::xs') => reverse(xs') @ [x]

fun replaceTildefromS([] : string list) = []
  | replaceTildefromS(h::t) = if ( (ord(String.sub(h,0)) = 34 andalso ord(String.sub(h,String.size(h)-1)) = 34) ) then String.substring(h,1,String.size(h)-2)::replaceTildefromS(t)
  else if String.sub(h, 0) = #"~" then replaceTilde(explode h)::replaceTildefromS(t)
  else h::replaceTildefromS(t)

fun getlet([]: string list, (lol ,forIndex) ) = (lol,forIndex)
  | getlet(h::t,(lol,forIndex)) = let
    fun loopy([] : string list,lol,forIndex,letlist,letCount) = []
      | loopy(head::tail,lol,forIndex,letlist,letCount) =
        if head = "let" then let
          val letlist = head::letlist
          val letCount = letCount + 1
        in
          loopy(tail,lol,forIndex,letlist,letCount)
        end
        else if head = "end" andalso letCount = 0 then let
          val letlist = reverse (letlist)
          val lol = letlist::lol
        in
          lol
        end
        else if head = "end" then let
          val letlist = head::letlist
          val letCount = letCount - 1
        in
          loopy(tail,lol,forIndex,letlist,letCount)
        end
        else let
          val letlist = head::letlist
        in
          loopy(tail,lol,forIndex,letlist,letCount)
        end

  in
    if (h = "let" andalso forIndex <> List.length(t) ) then let
      val lol = loopy(t,lol,forIndex,[],0)
      val forIndex = forIndex + 1
    in
      getlet(t,(lol,forIndex))
    end
    else if (h <> "let" andalso forIndex <> List.length(t) ) then let
      val forIndex = forIndex + 1
    in
      getlet(t,(lol,forIndex))
    end
    else getlet(t,(lol,forIndex))
  end

fun getfun([]: string list, (lol, funCount) ) = (lol, funCount)
    | getfun(h::t, (lol, funCount)) =
        if String.isSubstring "funEnd" h andalso (funCount - 1 = 0) then ( lol,funCount)
        else if String.isSubstring "fun" h orelse String.isSubstring "inOutFun" h then let
          val funCount = funCount + 1
        in
          getfun(t, (lol,funCount))
        end
        else let
          val lol = h::lol
        in
        getfun(t,(lol,funCount))
        end

fun check line (s, dic, letend, i, letCount, islet, fundic) =
  if String.isSubstring "quit" line then (s,dic, letend, i, letCount, islet, fundic)

  else if String.isSubstring "push" line then
    ( if String.isSubstring "-0" line then ("0"::s,dic, letend, i, letCount, islet, fundic)
      else if inDic(dic,String.extract(line,5,NONE)) then (String.extract(line,5,NONE)::s,dic, letend, i, letCount, islet, fundic)
      else if isInt( replaceNeg( explode(String.extract(line,5,NONE)) ) ) then (implode( replaceNeg( explode(String.extract(line,5,NONE)) ) )::s, dic, letend, i, letCount, islet, fundic) (*check if it is a int*)
      else if isalnum( explode(String.extract(line,5,NONE)) ) then (String.extract(line,5,NONE)::s,((String.extract(line,5,NONE)),"None")::dic, letend, i, letCount, islet, fundic) (*check if it is a name*)
      else if (ord(String.sub(line,5)) = 34 andalso ord(String.sub(line,String.size(line)-1)) = 34) then (String.substring(line,5,String.size(line)-5)::s,dic, letend, i, letCount, islet, fundic)(*check if it is a string*)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    )

  else if line = "pop" then
    (if List.length(s) <> 0 then (tl s,dic, letend, i, letCount, islet, fundic) else (":error:"::s,dic, letend, i, letCount, islet, fundic) )

  else if line = ":true:"  then (":true:"::s,dic, letend, i, letCount, islet, fundic)

  else if String.isSubstring ":false:" line then (":false:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "add" then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = Int.toString( valOf(Int.fromString(hd s)) + valOf(Int.fromString(List.nth(s,1))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if ( isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = Int.toString( valOf(Int.fromString(getVal(dic,hd s))) + valOf(Int.fromString(List.nth(s,1))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode( hd s ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) ) then let
          val x = Int.toString( valOf(Int.fromString(hd s)) + valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) ) then let
          val x = Int.toString( valOf(Int.fromString(getVal(dic,hd s))) + valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "sub" then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) - valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if ( isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) - valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode( hd s ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) - valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) - valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "mul"  then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) * valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if ( isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) * valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode( hd s ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) * valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) * valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "div"  then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode(List.nth(s,1)) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) div valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if (  isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode(List.nth(s,1)) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) div valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if (  isInt( explode( hd s ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) div valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if (  isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) div valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "rem"  then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode(List.nth(s,1)) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) mod valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if (  isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode(List.nth(s,1)) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString(List.nth(s,1))) mod valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if (  isInt( explode( hd s ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) mod valOf(Int.fromString(hd s)) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if (  isInt( explode( getVal(dic,hd s) ) ) andalso isInt( explode( getVal(dic,List.nth(s,1))) ) andalso hd s <> "0" ) then let
          val x = Int.toString( valOf(Int.fromString( getVal(dic,List.nth(s,1)) ) ) mod valOf(Int.fromString(getVal(dic,hd s))) )
          val s = List.drop( List.drop(s,1), 1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "neg"  then
    if List.length(s) > 0 then
      if inDic(dic, hd s) = false then
        if isInt(explode(hd s)) then let
          val x = Int.toString( valOf(Int.fromString(hd s)) * ~1 )
          val s = List.drop(s,1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if inDic(dic, hd s) then
        if isInt(explode(getVal(dic,hd s))) then let
          val x = Int.toString( valOf(Int.fromString(getVal(dic,hd s))) * ~1 )
          val s = List.drop(s,1)
        in
          (x::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "swap"  then
    (if List.length(s) > 1 then let
      val x = hd s
      val y = List.nth(s,1)
      val s = List.drop( List.drop(s,1), 1)
    in
      (y::x::s,dic, letend, i, letCount, islet, fundic)
    end
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    )

  else if line = "and"  then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isBool( hd s ) andalso isBool( List.nth(s,1) )) then let
          val x = (hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" andalso y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if ( isBool( getVal(dic,hd s) ) andalso isBool( List.nth(s,1) )) then let
          val x = getVal(dic,hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" andalso y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isBool( hd s ) andalso isBool( getVal(dic,List.nth(s,1)) )) then let
          val x = (hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" andalso y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isBool( getVal(dic,hd s) ) andalso isBool( getVal(dic,List.nth(s,1)) )) then let
          val x = getVal(dic,hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" andalso y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "or"  then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isBool( hd s ) andalso isBool( List.nth(s,1) )) then let
          val x = (hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" orelse y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if ( isBool( getVal(dic,hd s) ) andalso isBool( List.nth(s,1) )) then let
          val x = getVal(dic,hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" orelse y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isBool( hd s ) andalso isBool( getVal(dic,List.nth(s,1)) )) then let
          val x = (hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" orelse y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isBool( getVal(dic,hd s) ) andalso isBool( getVal(dic,List.nth(s,1)) )) then let
          val x = getVal(dic,hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = ":true:" orelse y = ":true:" then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "not"  then
    if List.length(s) > 0 then
      if inDic(dic, hd s) = false then
        if (isBool(hd s)) then let
          val x = (hd s)
          val s = List.drop(s,1)
        in
          if x = ":true:" then (":false:"::s,dic, letend, i, letCount, islet, fundic)
          else (":true:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if inDic(dic, hd s) then
        if isBool(getVal(dic,hd s)) then let
          val x = (getVal(dic,hd s))
          val s = List.drop(s,1)
        in
          if x = ":true:" then (":false:"::s,dic, letend, i, letCount, islet, fundic)
          else (":true:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "equal" then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = (hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if ( isInt( explode(getVal(dic,hd s)) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = getVal(dic,hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode( getVal(dic,List.nth(s,1)) ) ) ) then let
          val x = (hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode(getVal(dic,hd s)) ) andalso isInt( explode( getVal(dic,List.nth(s,1)) ) ) ) then let
          val x = getVal(dic,hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x = y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "lessThan" then
    if List.length(s) > 1 then
      if ( inDic(dic, hd s) = false andalso inDic(dic, List.nth(s,1)) = false ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = (hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x > y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) )= false ) then
        if ( isInt( explode(getVal(dic,hd s)) ) andalso isInt( explode(List.nth(s,1)) ) ) then let
          val x = getVal(dic,hd s)
          val y = List.nth(s,1)
          val s = List.drop( List.drop(s,1), 1)
        in
          if x > y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s)= false andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode(hd s) ) andalso isInt( explode( getVal(dic,List.nth(s,1)) ) ) ) then let
          val x = (hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x > y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else if ( inDic(dic, hd s) andalso inDic(dic, List.nth(s,1) ) ) then
        if ( isInt( explode(getVal(dic,hd s)) ) andalso isInt( explode( getVal(dic,List.nth(s,1)) ) ) ) then let
          val x = getVal(dic,hd s)
          val y = getVal(dic,List.nth(s,1))
          val s = List.drop( List.drop(s,1), 1)
        in
          if x > y then
            (":true:"::s,dic, letend, i, letCount, islet, fundic)
          else (":false:"::s,dic, letend, i, letCount, islet, fundic)
        end
        else (":error:"::s,dic, letend, i, letCount, islet, fundic)
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "bind" then
    if List.length(s) > 1 then
      if (inDic(dic, hd s) andalso inDic(dic,List.nth(s,1))) then let
        val x = getVal(dic,hd s)
        val y = List.nth(s,1)
        val s = List.drop( List.drop(s,1), 1)
      in
        (":unit:"::s,(y,x)::dic, letend, i, letCount, islet, fundic)
      end
      else if (not (hd s = ":error:" ) andalso inDic(dic,List.nth(s,1)) ) then let
        val x = (hd s)
        val y = List.nth(s,1)
        val s = List.drop( List.drop(s,1), 1)
      in
        (":unit:"::s,(y,x)::dic, letend, i, letCount, islet, fundic)
      end
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "if" then
    if List.length(s) > 2 then
      if inDic(dic, List.nth(s,2)) = false andalso isBool(List.nth(s,2)) then let
          val x = hd s
          val y = List.nth(s,1)
          val z = List.nth(s,2)
          val s = List.drop(s,3)
      in
        if z = ":true:" then (x::s,dic, letend, i, letCount, islet, fundic)
        else (y::s,dic, letend, i, letCount, islet, fundic)
      end
      else if inDic(dic, List.nth(s,2)) andalso isBool(getVal(dic,List.nth(s,2))) then let
          val x = hd s
          val y = List.nth(s,1)
          val z = getVal(dic,List.nth(s,2))
          val s = List.drop(s,3)
      in
        if z = ":true:" then (x::s,dic, letend, i, letCount, islet, fundic)
        else (y::s,dic, letend, i, letCount, islet, fundic)
      end
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "let" then let
    val letLol = reverse ( #1 (getlet( letend ,([],0))) )
    (*val strend = ["end"]
    val head = "let"::(hd letLol)@strend
    val letlol = head::List.drop(letLol,1)*)
    (*val hi = printlol(letLol)*)
    val currentLetend = List.nth( letLol, letCount)
    (*val p = (print("[");printList(currentLetend))*)
    (*val hi = printList(currentLetend)*)
    fun looper([] : string list, (s: string list, dic, letend, i, letCount, islet, fundic)) = (s,dic, letend, i, letCount, islet, fundic)
      | looper(h::t, (s,dic, letend, i, letCount, islet, fundic)) = let
        val t = if islet = true then List.drop(letend, i+1) else t
        val h = if islet = true then "end" else h
        val islet = if islet = true then false else islet
        (*val hi = print("h is " ^ h ^ "\n")
        val hi = (print("[");printList(t))*)
        val i = i + 1

      in
        (looper(t, check h (s,dic, letend, i, letCount, islet, fundic)))
      end
    val ans = hd ( #1 (looper(currentLetend, ([],dic, currentLetend, 0, 0, false,[])) ))
    (*val hi = print("ans is " ^ ans ^ "\n")*)
    val i = i + List.length( currentLetend )-1
    (*val hi = print(Int.toString(i))*)
    val islet = true
    val letCount = letCount + 1
    in
      (ans::s,dic,letend,i, letCount, islet, fundic)
    end

  else if line = "end" then (s,dic,letend,i, letCount, islet, fundic)

  else if line = "funEnd"  then (s,dic,letend,i, letCount, islet, fundic)

  else if String.isSubstring "fun" line then let
    val tokenList = String.tokens (fn x => x = #" ")line
    val funName = List.nth(tokenList,1)
    val funPram = List.nth(tokenList,2)
    val funtoEnd = reverse (#1 (getfun(List.drop(letend,i-1) ,([],0))))
    val isInOut = false
    val temp = (funName,funtoEnd,dic,funPram,isInOut)
    val fundic = temp::fundic
    val i = i + List.length( funtoEnd )-1
    val islet = true
    in
      (":unit:"::s,dic, letend, i, letCount, islet, fundic)
    end

  else if line = "funEnd" then (s,dic,letend,i, letCount, islet, fundic)

  else if line = "call"  then
    if List.length(s) > 1 then
      if infunDic(fundic, hd s) andalso infunDic(fundic, List.nth(s,1)) then let
        fun looper([] : string list, (s: string list, dic, letend, i, letCount, islet, fundic)) = (s,dic, letend, i, letCount, islet, fundic)
          | looper(h::t, (s,dic, letend, i, letCount, islet, fundic)) = let
            val t = if islet = true then List.drop(letend, i+1) else t
            val h = if islet = true then "end" else h
            val islet = if islet = true then false else islet
            val i = i + 1

          in
            (looper(t, check h (s,dic, letend, i, letCount, islet, fundic)))
          end
        val funtoEnd = getfuntoEnd(fundic, hd s)
        val tempdic = getdic(fundic, hd s)
        val tempfundic = (getfunPram(fundic,hd s),getfuntoEnd(fundic,List.nth(s,1)),getdic(fundic,List.nth(s,1)),getfunPram(fundic,List.nth(s,1)), getisInOut(fundic,List.nth(s,1)))::fundic
        val ans = hd( #1( looper(funtoEnd, ([],tempdic, funtoEnd, 0, 0, false, tempfundic) )) )
        val s = List.drop( List.drop(s,1), 1)
        in
          (ans::s,dic,letend,i, letCount, islet, fundic)
        end
      else if infunDic(fundic, hd s) andalso inDic(dic, List.nth(s,1)) then let
        fun looper([] : string list, (s: string list, dic, letend, i, letCount, islet, fundic)) = (s,dic, letend, i, letCount, islet, fundic)
          | looper(h::t, (s,dic, letend, i, letCount, islet, fundic)) = let
            val t = if islet = true then List.drop(letend, i+1) else t
            val h = if islet = true then "end" else h
            val islet = if islet = true then false else islet
            val i = i + 1

          in
            (looper(t, check h (s,dic, letend, i, letCount, islet, fundic)))
          end
        val funtoEnd = getfuntoEnd(fundic, hd s)
        val tempdic = getdic(fundic, hd s)
        val tempdic = ( getfunPram(fundic,hd s), getVal(dic, List.nth(s,1) ))::tempdic
        val ans = hd( #1( looper(funtoEnd, ([],tempdic, funtoEnd, 0, 0, false, fundic) )) )
        val s = List.drop( List.drop(s,1), 1)
        in
          (ans::s,dic,letend,i, letCount, islet, fundic)
        end
        else if infunDic(fundic, hd s) andalso inDic(dic, List.nth(s,1)) = false then let
          fun looper([] : string list, (s: string list, dic, letend, i, letCount, islet, fundic)) = (s,dic, letend, i, letCount, islet, fundic)
            | looper(h::t, (s,dic, letend, i, letCount, islet, fundic)) = let
              val t = if islet = true then List.drop(letend, i+1) else t
              val h = if islet = true then "end" else h
              val islet = if islet = true then false else islet
              val i = i + 1

            in
              (looper(t, check h (s,dic, letend, i, letCount, islet, fundic)))
            end
          val funtoEnd = getfuntoEnd(fundic, hd s)
          val tempdic = getdic(fundic, hd s)
          val tempdic = ( getfunPram(fundic,hd s), List.nth(s,1) )::tempdic
          val ans = hd( #1( looper(funtoEnd, ([],tempdic, funtoEnd, 0, 0, false, fundic) )) )
          val s = List.drop( List.drop(s,1), 1)
          in
            (ans::s,dic,letend,i, letCount, islet, fundic)
          end
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)

    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else if line = "return" then
    if List.length(s) > 0 then
      if inDic(dic, hd s) = false then (s,dic,letend,i, letCount, islet, fundic)
      else if inDic(dic, hd s) then let
        val s = getVal(dic, hd s)::s
      in
        (s,dic,letend,i, letCount, islet, fundic)
      end
      else (":error:"::s,dic, letend, i, letCount, islet, fundic)
    else (":error:"::s,dic, letend, i, letCount, islet, fundic)

  (*else if String.isSubstring "inOutFun" line then let
    val tokenList = String.tokens (fn x => x = #" ")line
    val funName = List.nth(tokenList,1)
    val funPram = List.nth(tokenList,2)
    val funtoEnd = reverse (#1 (getfun(List.drop(letend,i-1) ,([],0))))
    val isInOut = true
    val temp = (funName,funtoEnd,dic,funPram,isInOut)
    val i = i + List.length( funtoEnd )-1
    val islet = true
    in
      (":unit:"::s,dic, letend, i, letCount, islet, fundic)
    end*)

  else if line = ":error:" then (":error:"::s,dic, letend, i, letCount, islet, fundic)

  else (":error:"::s,dic, letend, i, letCount, islet, fundic)

fun iter([] : string list, (s: string list, dic, letend, i, letCount, islet, fundic)) = (s,dic, letend, i, letCount, islet, fundic)
  | iter(h::t, (s,dic, letend, i, letCount, islet, fundic)) = let
    (*val hi = (print("[ ");printList(t))*)
    val t = if islet = true then (List.drop(letend, i+1)) else t
    val h = if islet = true then "end" else h
    val islet = if islet = true then false else islet
    (*val hi = print("h is " ^ h ^ "\n")
    val hi = (print("[");printList(t))*)
    (*val hi = print("letCount is " ^ Int.toString(letCount) ^ "\n")*)

    (*val hi = print("i is " ^ Int.toString(i) ^ "\n")*)

    val i = i + 1

  in
    (
      (*print("i is " ^ Int.toString(i) ^ "\n");*)
    iter(t, check h (s,dic, letend, i, letCount, islet, fundic)))
  end

fun reader(input : string, output : string) = let
    val inStream = TextIO.openIn(input)
    val outStream = TextIO.openOut(output)
    val readLine = TextIO.inputLine(inStream)
    fun helper(readLine : string option) =
      case readLine of
        NONE => []
        | SOME(c) =>
          implode( stripNewline(explode(c)) )::helper(TextIO.inputLine inStream)
  in
    helper(readLine)
  end

(*val x = reader("sample_input1.txt","sample_output1.txt")
val stack = replaceTildefromS( iter(x,[]) )*)

fun interpreter(input : string, output : string) =
  let
    val inStream = TextIO.openIn(input)
    val outStream = TextIO.openOut(output)
    val letend = reader(input,output)
    (*val letend = reverse ( #1 (getlet(reader(input,output) ,([],0))) )*)
    val s = replaceTildefromS( #1 (iter(reader(input,output), ([],[], letend, 0, 0, false,[]) )) )
    (*val dict = #2 (iter(reader(input,output), ([],[], letend, 0, 0, false) ))*)
    fun helper([] : string list) = ( TextIO.closeIn(inStream); TextIO.closeOut(outStream) )
        | helper(h::t) =
            (
            (*printList(s);*)
            (*printDic(dict);*)
            TextIO.output(outStream, h^"\n");
            helper(t) )

  in
    helper(s)
  end

  (*val _ = interpreter("input_1.txt","output_1.txt")*)
