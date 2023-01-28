open Pdfutil

let glyphlist_src = __DATA:glyphlist.txt

let parse_glyphlist i =
  let parse_line l =
    let len = String.length l in
    if len = 0 then None else
    if l.[0] = '#' then None else
    (* Find semicolon. Bail or extract name *)
    let semipos =
      try Some (String.index l ';') with Not_found -> None
    in
      match semipos with
      | None -> None
      | Some semipos ->
          let name = String.sub l 0 semipos in
          (* Read codes, one or more, space-delimited, four chars each. *)
          let codes = ref []
          and pos = ref (semipos + 1) in
            try
              while true do
                codes := int_of_string ("0x" ^ String.sub l !pos 4)::!codes;
                pos += 5
              done;
              Some ("", [])
            with
              _ -> Some ("/" ^ name, rev !codes)
  in
    let out = ref [] in
    try
      while true do
        match parse_line (Pdfio.read_line i) with
        | None -> ()
        | Some (s, is) -> out := (s, is)::!out
      done;
      []
    with
      End_of_file -> rev !out

let glyphmap =
  memoize
    (fun () ->
       parse_glyphlist
         (Pdfio.input_of_string
           (Pdfio.string_of_bytes
             (Pdfcodec.decode_flate
               (Pdfio.bytes_of_string glyphlist_src)))))

(*let _ =
  iter
   (fun (k, v) ->
      Printf.printf "%s = " k;
      iter (Printf.printf "%X ") v;
      flprint "\n")
   glyphmap*)

let name_to_pdf =
  [
  (* New items from ISO Standard D.3... These appear in annotation text, even though it's a text string. *)
  (* FIXME: Should we add all the codepoints, even those called undefined, to allow failures to be tracked? *)
  "/controlCR", 0o015; "/controlLF", 0o012; "/controlHT", 0o013;
  (* Original items from 1.6 spec *)
  "/A", 0o101; "/AE", 0o306; "/Aacute", 0o301; "/Acircumflex", 0o302;
  "/Adieresis", 0o304; "/Agrave", 0o300; "/Aring", 0o305; "/Atilde", 0o303;
  "/B", 0o102; "/C", 0o103; "/Ccedilla", 0o307; "/D", 0o104; "/E", 0o105;
  "/Eacute", 0o311; "/Ecircumflex", 0o312; "/Edieresis", 0o313; "/Egrave",
  0o310; "/Eth", 0o320; "/Euro", 0o240; "/F", 0o106; "/G", 0o107; "/H", 0o110;
  "/I", 0o111; "/Iacute", 0o315; "/Icircumflex", 0o316; "/Idieresis", 0o317;
  "/Igrave", 0o314; "/J", 0o112; "/K", 0o113; "/L", 0o114; "/Lslash", 0o225;
  "/M", 0o115; "/N", 0o116; "/Ntilde", 0o321; "/O", 0o117; "/OE", 0o226;
  "/Oacute", 0o323; "/Ocircumflex", 0o324; "/Odieresis", 0o326; "/Ograve",
  0o322; "/Oslash", 0o330; "/Otilde", 0o325; "/P", 0o120; "/Q", 0o121; "/R",
  0o122; "/S", 0o123; "/Scaron", 0o227; "/T", 0o124; "/Thorn", 0o336; "/U",
  0o125; "/Uacute", 0o332; "/Ucircumflex", 0o333; "/Udieresis", 0o334;
  "/Ugrave", 0o331; "/V", 0o126; "/W", 0o127; "/X", 0o130; "/Y", 0o131;
  "/Yacute", 0o335; "/Ydieresis", 0o230; "/Z", 0o132; "/Zcaron", 0o231; "/a",
  0o141; "/aacute", 0o341; "/acircumflex", 0o342; "/acute", 0o264;
  "/adieresis", 0o344; "/ae", 0o346; "/agrave", 0o340; "/ampersand", 0o046;
  "/aring", 0o345; "/asciicircum", 0o136; "/asciitilde", 0o176; "/asterisk",
  0o052; "/at", 0o100; "/atilde", 0o343; "/b", 0o142; "/backslash", 0o134;
  "/bar", 0o174; "/braceleft", 0o173; "/braceright", 0o175; "/bracketleft",
  0o133; "/bracketright", 0o135; "/breve", 0o030; "/brokenbar", 0o246;
  "/bullet", 0o200; "/c", 0o143; "/caron", 0o031; "/ccedilla", 0o347;
  "/cedilla", 0o270; "/cent", 0o242; "/circumflex", 0o032; "/colon", 0o072;
  "/comma", 0o054; "/copyright", 0o251; "/currency", 0o244; "/d", 0o144;
  "/dagger", 0o201; "/daggerdbl", 0o202; "/degree", 0o260; "/dieresis", 0o250;
  "/divide", 0o367; "/dollar", 0o044; "/dotaccent", 0o033; "/dotlessi", 0o232;
  "/e", 0o145; "/eacute", 0o351; "/ecircumflex", 0o352; "/edieresis", 0o353;
  "/egrave", 0o350; "/eight", 0o070; "/ellipsis", 0o203; "/emdash", 0o204;
  "/endash", 0o205; "/equal", 0o075; "/eth", 0o360; "/exclam", 0o041;
  "/exclamdown", 0o241; "/f", 0o146; "/fi", 0o223; "/five", 0o065; "/fl",
  0o223; "/florin", 0o206; "/four", 0o064; "/fraction", 0o207; "/g", 0o147;
  "/germandbls", 0o337; "/grave", 0o140; "/greater", 0o076; "/guillemotleft",
  0o253; "/guillemotright", 0o273; "/guilsinglleft", 0o210; "/guilsinglright",
  0o211; "/h", 0o150; "/hungarumlaut", 0o034; "/hyphen", 0o055; "/i", 0o151;
  "/iacute", 0o355; "/icircumflex", 0o356; "/idieresis", 0o357; "/igrave",
  0o354; "/j", 0o152; "/k", 0o153; "/l", 0o154; "/less", 0o074; "/logicalnot",
  0o254; "/lslash", 0o233; "/m", 0o155; "/macron", 0o257; "/minus", 0o212;
  "/mu", 0o265; "/multiply", 0o327; "/n", 0o156; "/nine", 0o071; "/ntilde",
  0o361; "/numbersign", 0o043; "/o", 0o157; "/oacute", 0o363; "/ocircumflex",
  0o364; "/odieresis", 0o366; "/oe", 0o234; "/ogonek", 0o035; "/ograve", 0o362;
  "/one", 0o061; "/onehalf", 0o275; "/onequarter", 0o274; "/onesuperior",
  0o271; "/ordfeminine", 0o252; "/ordmasculine", 0o272; "/oslash", 0o370;
  "/otilde", 0o365; "/p", 0o160; "/paragraph", 0o266; "/parenleft", 0o050;
  "/parenright", 0o051; "/percent", 0o045; "/period", 0o056; "/periodcentered",
  0o267; "/perthousand", 0o213; "/plus", 0o053; "/plusminus", 0o261; "/q",
  0o161; "/question", 0o077; "/questiondown", 0o277; "/quotedbl", 0o042;
  "/quotedblbase", 0o214; "/quotedblleft", 0o215; "/quotedblright", 0o216;
  "/quoteleft", 0o217; "/quoteright", 0o220; "/quotesinglbase", 0o221;
  "/quotesingle", 0o047; "/r", 0o162; "/registered", 0o256; "/ring", 0o036;
  "/s", 0o163; "/scaron", 0o235; "/section", 0o247; "/semicolon", 0o073;
  "/seven", 0o067; "/six", 0o066; "/slash", 0o057; "/space", 0o040;
  "/sterling", 0o243; "/t", 0o164; "/thorn", 0o376; "/three", 0o063;
  "/threequarters", 0o276; "/threesuperior", 0o263; "/tilde", 0o037;
  "/trademark", 0o222; "/two", 0o062; "/twosuperior", 0o262; "/u", 0o165;
  "/uacute", 0o372; "/ucircumflex", 0o373; "/udieresis", 0o374; "/ugrave",
  0o371; "/underscore", 0o137; "/v", 0o166; "/w", 0o167; "/x", 0o170; "/y",
  0o171; "/yacute", 0o375; "/ydieresis", 0o377; "/yen", 0o245; "/z", 0o172;
  "/zcaron", 0o236; "/zero", 0o060]

(* Standard encoding *)
let name_to_standard =
  ["/A", 0o101; "/AE", 0o341; "/B", 0o102; "/C", 0o103; "/D", 0o104; "/E",
  0o105; "/F", 0o106; "/G", 0o107; "/H", 0o110; "/I", 0o111; "/J", 0o112; "/K",
  0o113; "/L", 0o114; "/Lslash", 0o350; "/M", 0o115; "/N", 0o116; "/O", 0o117;
  "/OE", 0o352; "/Oslash", 0o351; "/P", 0o120; "/Q", 0o121; "/R", 0o122; "/S",
  0o123; "/T", 0o124; "/U", 0o125; "/V", 0o126; "/W", 0o127; "/X", 0o130; "/Y",
  0o131; "/Z", 0o132; "/a", 0o141; "/acute", 0o302; "/ae", 0o361; "/ampersand",
  0o046; "/asciicircum", 0o136; "/asciitilde", 0o176; "/asterisk", 0o052; "/at",
  0o100; "/b", 0o142; "/backslash", 0o134; "/bar", 0o174; "/braceleft", 0o173;
  "/braceright", 0o175; "/bracketleft", 0o133; "/bracketright", 0o135; "/breve",
  0o306; "/bullet", 0o267; "/c", 0o143; "/caron", 0o317; "/cedilla", 0o313;
  "/cent", 0o242; "/circumflex", 0o303; "/colon", 0o072; "/comma", 0o054;
  "/currency", 0o250; "/d", 0o144; "/dagger", 0o262; "/daggerdbl", 0o263;
  "/dieresis", 0o310; "/dollar", 0o044; "/dotaccent", 0o307; "/dottlessi",
  0o365; "/e", 0o145; "/eight", 0o070; "/ellipsis", 0o274; "/emdash", 0o320;
  "/endash", 0o261; "/equal", 0o075; "/exclam", 0o041; "/exclamdown", 0o241;
  "/f", 0o146; "/fi", 0o256; "/five", 0o065; "/fl", 0o257; "/florin", 0o246;
  "/four", 0o064; "/fraction", 0o244; "/g", 0o147; "/germandbls", 0o373;
  "/grave", 0o301; "/greater", 0o076; "/guillemotleft", 0o253;
  "/guillemotright", 0o273; "/guilsinglleft", 0o254; "/guilsinglright", 0o255;
  "/h", 0o150; "/hungarumlaut", 0o315; "/hyphen", 0o055; "/i", 0o151; "/j",
  0o152; "/k", 0o153; "/l", 0o154; "/less", 0o074; "/lslash", 0o370; "/m",
  0o155; "/macron", 0o305; "/n", 0o156; "/nine", 0o071; "/numbersign", 0o043;
  "/o", 0o157; "/oe", 0o372; "/ogonek", 0o316; "/one", 0o061; "/ordfeminine",
  0o343; "/ordmasculine", 0o353; "/oslash", 0o361; "/p", 0o160; "/paragraph",
  0o266; "/parenleft", 0o050; "/parenright", 0o051; "/percent", 0o045;
  "/period", 0o056; "/periodcentered", 0o264; "/perthousand", 0o275; "/plus",
  0o053; "/q", 0o161; "/question", 0o077; "/questiondown", 0o277; "/quotedbl",
  0o042; "/quotedblbase", 0o271; "/quotedblleft", 0o252; "/quotedblright",
  0o272; "/quoteleft", 0o140; "/quoteright", 0o047; "/quotesinglbase", 0o270;
  "/quotesingle", 0o251; "/r", 0o162; "/ring", 0o312; "/s", 0o163; "/section",
  0o247; "/semicolon", 0o073; "/seven", 0o067; "/six", 0o066; "/slash", 0o057;
  "/space", 0o040; "/sterling", 0o243; "/t", 0o164; "/three", 0o063; "/tilde",
  0o304; "/two", 0o062; "/u", 0o165; "/underscore", 0o137; "/v", 0o166; "/w",
  0o167; "/x", 0o170; "/y", 0o171; "/yen", 0o245; "/z", 0o172; "/zero", 0o060]
 
(* Mac Roman Encoding *)
let name_to_macroman =
  ["/A", 0o101; "/AE", 0o256; "/Aacute", 0o347; "/Acircumflex", 0o345;
  "/Adieresis", 0o200; "/Agrave", 0o313; "/Aring", 0o201; "/Atilde", 0o314;
  "/B", 0o102; "/C", 0o103; "/Ccedilla", 0o202; "/D", 0o104; "/E", 0o105;
  "/Eacute", 0o203; "/Ecircumflex", 0o346; "/Edieresis", 0o350; "/Egrave",
  0o351; "/F", 0o106; "/G", 0o107; "/H", 0o110; "/I", 0o111; "/Iacute", 0o352;
  "/Icircumflex", 0o353; "/Idieresis", 0o354; "/Igrave", 0o355; "/J", 0o112;
  "/K", 0o113; "/L", 0o114; "/M", 0o115; "/N", 0o116; "/Ntilde", 0o204; "/O",
  0o117; "/OE", 0o316; "/Oacute", 0o356; "/Ocircumflex", 0o357; "/Odieresis",
  0o205; "/Ograve", 0o361; "/Oslash", 0o257; "/Otilde", 0o315; "/P", 0o120;
  "/Q", 0o121; "/R", 0o122; "/S", 0o123; "/T", 0o124; "/U", 0o125; "/Uacute",
  0o362; "/Ucircumflex", 0o363; "/Udieresis", 0o206; "/Ugrave", 0o364; "/V",
  0o126; "/W", 0o127; "/X", 0o130; "/Y", 0o131; "/Ydieresis", 0o331; "/Z",
  0o132; "/a", 0o141; "/aacute", 0o207; "/acircumflex", 0o211; "/acute", 0o253;
  "/adieresis", 0o212; "/ae", 0o276; "/agrave", 0o210; "/ampersand", 0o046;
  "/aring", 0o214; "/asciicircum", 0o136; "/asciitilde", 0o176; "/asterisk",
  0o052; "/at", 0o100; "/atilde", 0o213; "/b", 0o142; "/backslash", 0o134;
  "/bar", 0o174; "/braceleft", 0o173; "/braceright", 0o175; "/bracketleft",
  0o133; "/bracketright", 0o135; "/breve", 0o371; "/bullet", 0o245; "/c", 0o143;
  "/caron", 0o377; "/ccedilla", 0o215; "/cedilla", 0o374; "/cent", 0o242;
  "/circumflex", 0o366; "/colon", 0o072; "/comma", 0o054; "/copyright", 0o251;
  "/currency", 0o333; "/d", 0o144; "/dagger", 0o240; "/daggerdbl", 0o340;
  "/degree", 0o241; "/dieresis", 0o254; "/divide", 0o326; "/dollar", 0o044;
  "/dotaccent", 0o372; "/dotlessi", 0o365; "/e", 0o145; "/eacute", 0o216;
  "/ecircumflex", 0o220; "/edieresis", 0o221; "/egrave", 0o217; "/eight", 0o070;
  "/ellipsis", 0o311; "/emdash", 0o321; "/endash", 0o320; "/equal", 0o075;
  "/exclam", 0o041; "/exclamdown", 0o301; "/f", 0o146; "/fi", 0o336; "/five",
  0o065; "/fl", 0o337; "/florin", 0o304; "/four", 0o064; "/fraction", 0o332;
  "/g", 0o147; "/germandbls", 0o247; "/grave", 0o140; "/greater", 0o076;
  "/guillemotleft", 0o307; "/guillemotright", 0o310; "/guilsinglleft", 0o334;
  "/guilsinglright", 0o335; "/h", 0o150; "/hungrumlaut", 0o375; "/hyphen",
  0o055; "/i", 0o151; "/iacute", 0o222; "/icircumflex", 0o224; "/idieresis",
  0o225; "/igrave", 0o223; "/j", 0o152; "/k", 0o153; "/l", 0o154; "/less",
  0o074; "/logicalnot", 0o302; "/m", 0o155; "/macron", 0o370; "/mu", 0o265;
  "/n", 0o156; "/nine", 0o071; "/ntilde", 0o226; "/numbersign", 0o043; "/o",
  0o157; "/oacute", 0o227; "/ocircumflex", 0o231; "/odieresis", 0o232; "/oe",
  0o317; "/ogonek", 0o376; "/one", 0o061; "/ordfeminine", 0o273;
  "/ordmasculine", 0o274; "/oslash", 0o277; "/otilde", 0o233; "/p", 0o160;
  "/paragraph", 0o246; "/parenleft", 0o050; "/parenright", 0o051; "/percent",
  0o045; "/period", 0o056; "/periodcentered", 0o341; "/perthousand", 0o344;
  "/plus", 0o053; "/plusminus", 0o261; "/q", 0o161; "/question", 0o077;
  "/questiondown", 0o300; "/quotedbl", 0o042; "/quotedblbase", 0o343;
  "/quotedblleft", 0o322; "/quotedblright", 0o323; "/quoteleft", 0o324;
  "/quoteright", 0o325; "/quotesinglbase", 0o342; "/quotesingle", 0o047; "/r",
  0o162; "/registered", 0o250; "/ring", 0o373; "/s", 0o163; "/section", 0o244;
  "/semicolon", 0o073; "/seven", 0o067; "/six", 0o066; "/slash", 0o057;
  "/space", 0o040; "/sterling", 0o243; "/t", 0o164; "/three", 0o063; "/tilde",
  0o367; "/trademark", 0o252; "/two", 0o062; "/u", 0o165; "/uacute", 0o234;
  "/ucircumflex", 0o236; "/udieresis", 0o237; "/ugrave", 0o235; "/underscore",
  0o137; "/v", 0o166; "/w", 0o167; "/x", 0o170; "/y", 0o171; "/ydieresis",
  0o330; "/yen", 0o264; "/z", 0o172; "/zero", 0o060; (*"/space", 0o312*)] (*FIXME: see below*)

(* Win Ansi Encoding *)
let name_to_win =
  ["/A", 0o101; "/AE", 0o306; "/Aacute", 0o301; "/Acircumflex", 0o302;
  "/Adieresis", 0o304; "/Agrave", 0o300; "/Aring", 0o305; "/Atilde", 0o303;
  "/B", 0o102; "/C", 0o103; "/Ccedilla", 0o307; "/D", 0o104; "/E", 0o105;
  "/Eacute", 0o311; "/Ecircumflex", 0o312; "/Edieresis", 0o313; "/Egrave",
  0o310; "/Eth", 0o320; "/Euro", 0o200; "/F", 0o106; "/G", 0o107; "/H", 0o110;
  "/I", 0o111; "/Iacute", 0o315; "/Icircumflex", 0o316; "/Idieresis", 0o317;
  "/Igrave", 0o314; "/J", 0o112; "/K", 0o113; "/L", 0o114; "/M", 0o115; "/N",
  0o116; "/Ntilde", 0o321; "/O", 0o117; "/OE", 0o214; "/Oacute", 0o323;
  "/Ocircumflex", 0o324; "/Odieresis", 0o326; "/Ograve", 0o322; "/Oslash",
  0o330; "/Otilde", 0o325; "/P", 0o120; "/Q", 0o121; "/R", 0o122; "/S", 0o123;
  "/Scaron", 0o212; "/T", 0o124; "/Thorn", 0o336; "/U", 0o125; "/Uacute", 0o332;
  "/Ucircumflex", 0o333; "/Udieresis", 0o334; "/Ugrave", 0o331; "/V", 0o126;
  "/W", 0o127; "/X", 0o130; "/Y", 0o131; "/Yacute", 0o335; "/Ydieresis", 0o237;
  "/Z", 0o132; "/Zcaron", 0o216; "/a", 0o141; "/aacute", 0o341; "/acircumflex",
  0o342; "/acute", 0o264; "/adieresis", 0o344; "/ae", 0o346; "/agrave", 0o340;
  "/ampersand", 0o046; "/aring", 0o345; "/asciicircum", 0o136; "/asciitilde",
  0o176; "/asterisk", 0o052; "/at", 0o100; "/atilde", 0o343; "/b", 0o142;
  "/backslash", 0o134; "/bar", 0o174; "/braceleft", 0o173; "/braceright", 0o175;
  "/bracketleft", 0o133; "/bracketright", 0o135; "/brokenbar", 0o246; "/bullet",
  0o225; "/c", 0o143; "/ccedilla", 0o347; "/cedilla", 0o270; "/cent", 0o242;
  "/circumflex", 0o210; "/colon", 0o072; "/comma", 0o054; "/copyright", 0o251;
  "/currency", 0o244; "/d", 0o144; "/dagger", 0o206; "/daggerdbl", 0o207;
  "/degree", 0o260; "/dieresis", 0o250; "/divide", 0o367; "/dollar",0o044; "/e",
  0o145; "/eacute", 0o351; "/ecircumflex", 0o352; "/edieresis", 0o353;
  "/egrave", 0o350; "/eight", 0o070; "/ellipsis", 0o205; "/emdash", 0o227;
  "/endash", 0o226; "/equal", 0o075; "/eth", 0o360; "/exclam", 0o041;
  "/exclamdown", 0o241; "/f", 0o146; "/five", 0o065; "/florin", 0o203; "/four",
  0o064; "/g", 0o147; "/germandbls", 0o337; "/grave", 0o140; "/greater", 0o076;
  "/guillemotleft", 0o253; "/guillemotright", 0o273; "/guilsinglleft", 0o213;
  "/guilsinglright", 0o233; "/h", 0o150; "/hyphen", 0o055; "/i", 0o151;
  "/iacute", 0o355; "/icircumflex", 0o356; "/idieresis", 0o357; "/igrave",
  0o354; "/j", 0o152; "/k", 0o153; "/l", 0o154; "/less", 0o074; "/logicalnot",
  0o254; "/m", 0o155; "/macron", 0o257; "/mu", 0o265; "/multiply", 0o327; "/n",
  0o156; "/nine", 0o071; "/ntilde", 0o361; "/numbersign", 0o043; "/o", 0o157;
  "/oacute", 0o363; "/ocircumflex", 0o364; "/odieresis", 0o366; "/oe", 0o234;
  "/ograve", 0o362; "/one", 0o061; "/onehalf", 0o275; "/onequarter", 0o274;
  "/onesuperior", 0o271; "/ordfeminine", 0o252; "/ordmasculine", 0o272;
  "/oslash", 0o370; "/otilde", 0o365; "/p", 0o160; "/paragraph", 0o266;
  "/parenleft", 0o050; "/parenright", 0o051; "/percent", 0o045; "/period",
  0o056; "/periodcentered", 0o267; "/perthousand", 0o211; "/plus", 0o053;
  "/plusminus", 0o261; "/q", 0o161; "/question", 0o077; "/questiondown", 0o277;
  "/quotedbl", 0o042; "/quotedblbase", 0o204; "/quotedblleft", 0o223;
  "/quotedblright", 0o224; "/quoteleft", 0o221; "/quoteright", 0o222;
  "/quotesinglbase", 0o202; "/quotesingle", 0o047; "/r", 0o162; "/registered",
  0o256; "/s", 0o163; "/scaron", 0o232; "/section", 0o247; "/semicolon", 0o073;
  "/seven", 0o067; "/six", 0o066; "/slash", 0o057; "/space", 0o040; "/sterling",
  0o243; "/t", 0o164; "/thorn", 0o376; "/three", 0o063; "/threequarters", 0o276;
  "/threesuperior", 0o263; "/tilde", 0o230; "/trademark", 0o231; "/two", 0o062;
  "/twosuperior", 0o262; "/u", 0o165; "/uacute", 0o372; "/ucircumflex", 0o373;
  "/udieresis", 0o374; "/ugrave", 0o371; "/underscore", 0o137; "/v", 0o166;
  "/w", 0o167; "/x", 0o170; "/y", 0o171; "/yacute", 0o375; "/ydieresis", 0o377;
  "/yen", 0o245; "/z", 0o172; "/zcaron", 0o236; "/zero", 0o060;(* "/space", 0o240;
  "/hyphen", 0o255*)] (*FIXME: need these rules back in, but only in the reverse.. To make 1-to-1?*)

(* Mac Expert Encoding *)
let name_to_macexpert =
  ["/AEsmall", 0o276; "/Aacutesmall", 0o207; "/Acircumflexsmall", 0o211;
  "/Acutesmall", 0o047; "/Adieresissmall", 0o212; "/Agravesmall", 0o210;
  "/Aringsmall", 0o214; "/Asmall", 0o141; "/Atildesmall", 0o213; "/Brevesmall",
  0o363; "/Bsmall", 0o142; "/Caronsmall", 0o256; "/Ccedillasmall", 0o215;
  "/Cedillasmall", 0o311; "/Circumflexsmall", 0o136; "/Csmall", 0o143;
  "/Dieresissmall", 0o254; "/Dotaccentsmall", 0o372; "/Dsmall", 0o144;
  "/Eacutesmall", 0o216; "/Ecircumflexsmall", 0o220; "/Edieresissmall", 0o221;
  "/Egravesmall", 0o217; "/Esmall", 0o145; "/Ethsmall", 0o104; "/Fsmall", 0o146;
  "/Gravesmall", 0o140; "/Gsmall", 0o147; "/Hsmall", 0o150;
  "/Hungarumlautsmall", 0o042; "/Iacutesmall", 0o222; "/Icircumflexsmall",
  0o224; "/Idieresissmall", 0o225; "/Igravesmall", 0o223; "/Ismall", 0o151;
  "/Jsmall", 0o152; "/Ksmall", 0o153; "/Lslashsmall", 0o302; "/Lsmall", 0o154;
  "/Macronsmall", 0o364; "/Msmall", 0o155; "/Nsmall", 0o156; "/Ntildesmall",
  0o226; "/OEsmall", 0o317; "/Oacutesmall", 0o227; "/Ocircumflexsmall", 0o231;
  "/Odieresissmall", 0o232; "/Ogoneksmall", 0o362; "/Ogravesmall", 0o230;
  "/Oslashsmall", 0o277; "/Osmall", 0o157; "/Otildesmall", 0o233; "/Psmall",
  0o160; "/Qsmall", 0o161; "/Ringsmall", 0o373; "/Rsmall", 0o162;
  "/Scaronsmall", 0o247; "/Ssmall", 0o163; "/Thornsmall", 0o271; "/Tildesmall",
  0o176; "/Tsmall", 0o164; "/Uacutesmall", 0o234; "/Ucircumflexsmall", 0o236;
  "/Udieresissmall", 0o237; "/Ugravesmall", 0o235; "/Usmall", 0o165; "/Vsmall",
  0o166; "/Wsmall", 0o167; "/Xsmall", 0o170; "/Yacutesmall", 0o264;
  "/Ydieresissmall", 0o330; "/Ysmall", 0o171; "/Zcaronsmall", 0o275; "/Zsmall",
  0o172; "/ampersandsmall", 0o046; "/asuperior", 0o201; "/bsuperior", 0o365;
  "/centinferior", 0o251; "/centoldstyle", 0o043; "/centsuperior", 0o202;
  "/colon", 0o072; "/colonmonetary", 0o173; "/comma", 0o054; "/commainferior",
  0o262; "/commasuperior", 0o370; "/dollarinferior", 0o266; "/dollaroldstyle",
  0o044; "/dsuperior", 0o353; "/eightinferior", 0o245; "/eightoldstyle", 0o070;
  "/eightsuperior", 0o241; "/esuperior", 0o344; "/exclamdownsmall", 0o326;
  "/exclamsmall", 0o041; "/ff", 0o126; "/ffi", 0o131; "/ffl", 0o132; "/fi",
  0o127; "/figuredash", 0o320; "/fiveeighths", 0o114; "/fiveinferior", 0o260;
  "/fiveoldstyle", 0o065; "/fivesuperior", 0o336; "/fl", 0o130; "/fourinferior",
  0o242; "/fouroldstyle", 0o064; "/foursuperior", 0o335; "/fraction", 0o057;
  "/hyphen", 0o055; "/hypheninferior", 0o137; "/hyphensuperior", 0o137;
  "/isuperior", 0o351; "/lsuperior", 0o361; "/msuperior", 0o367;
  "/nineinferior", 0o273; "/nineoldstyle", 0o071; "/ninesuperior", 0o341;
  "/nsuperior", 0o366; "/onedotenleader", 0o053; "/oneeighth", 0o112;
  "/onefitted", 0o174; "/onehalf", 0o110; "/oneinferior", 0o301; "/oneoldstyle",
  0o061; "/onequarter", 0o107; "/onesuperior", 0o332; "/onethird", 0o116;
  "/osuperior", 0o257; "/parenleftinferior", 0o133; "/parenleftsuperior", 0o050;
  "/parenrightinferior", 0o135; "/parenrightsuperior", 0o051; "/period", 0o056;
  "/periodinferior", 0o263; "/periodsuperior", 0o371; "/questiondownsmall",
  0o300; "/questionsmall", 0o077; "/rsuperior", 0o345; "/rupiah", 0o175;
  "/semicolon", 0o073; "/seveneighths", 0o115; "/seveninferior", 0o246;
  "/sevenoldstyle", 0o067; "/sevensuperior", 0o340; "/sixinferior", 0o244;
  "/sixoldstyle", 0o066; "/sixsuperior", 0o337; "/space", 0o040; "/ssuperior",
  0o352; "/threeeighths", 0o113; "/threeinferior", 0o243; "/threeoldstyle",
  0o063; "/threequarters", 0o111; "/threequartersemdash", 0o075;
  "/threesuperior", 0o334; "/tsuperior", 0o346; "/twodotenleader", 0o052;
  "/twoinferior", 0o252; "/twooldstyle", 0o062; "/twosuperior", 0o333;
  "/twothirds", 0o117; "/zeroinferior", 0o274; "/zerooldstyle", 0o060;
  "/zerosuperior", 0o342]

(* Symbol Encoding *)
let name_to_symbol =
  ["/Alpha", 0o101; "/Beta", 0o102; "/Chi", 0o103; "/Delta", 0o104; "/Epsilon",
  0o105; "/Eta", 0o110; "/Euro", 0o240; "/Gamma", 0o107; "/Ifraktur", 0o301;
  "/Iota", 0o111; "/Kappa", 0o113; "/Lambda", 0o114; "/Mu", 0o115; "/Nu", 0o116;
  "/Omega", 0o127; "/Omicron", 0o117; "/Phi", 0o106; "/Pi", 0o120; "/Psi",
  0o131; "/Rfraktur", 0o302; "/Rho", 0o122; "/Sigma", 0o123; "/Tau", 0o124;
  "/Theta", 0o121; "/Upsilon", 0o125; "/Upsilon1", 0o241; "/Xi", 0o130; "/Zeta",
  0o132; "/aleph", 0o300; "/alpha", 0o141; "/ampersand", 0o046; "/angle", 0o320;
  "/angleleft", 0o341; "/angleright", 0o361; "/approxequal", 0o273;
  "/arrowboth", 0o253; "/arrowdblboth", 0o333; "/arrowdbldown", 0o337;
  "/arrowdblleft", 0o334; "/arrowdblright", 0o336; "/arrowhorizex", 0o276;
  "/arrowleft", 0o254; "/arrowright", 0o256; "/arrowup", 0o255; "/arrowvertex",
  0o275; "/asteriskmath", 0o052; "/bar", 0o174; "/beta", 0o142; "/braceleft",
  0o173; "/braceright", 0o175; "/bracelefttp", 0o354; "/braceleftmid", 0o355;
  "/braceleftbt", 0o376; "/bracerighttp", 0o374; "/bracerightmid", 0o375;
  "/bracerightbt", 0o376; "/braceex", 0o357; "/bracketleft", 0o133;
  "/bracketright", 0o135; "/bracketlefttp", 0o351; "/bracketleftex", 0o352;
  "/bracketleftbt", 0o353; "/bracketrighttp", 0o371; "/brackerrightex", 0o372;
  "/bracketrightbt", 0o373; "/bullet", 0o267; "/carriagereturn", 0o277; "/chi",
  0o143; "/circlemultiply", 0o304; "/circleplus", 0o305; "/club", 0o247;
  "/colon", 0o072; "/comma", 0o054; "/congruent", 0o100; "/copyrightsans",
  0o343; "/copyrightserif", 0o323; "/degree", 0o260; "/delta", 0o144;
  "/diamond", 0o250; "/divide", 0o270; "/dotmath", 0o327; "/eight", 0o070;
  "/element", 0o316; "/ellipsis", 0o274; "/emptyset", 0o306; "/epsilon", 0o145;
  "/equal", 0o075; "/equivalence", 0o272; "/eta", 0o150; "/exclam", 0o041;
  "/existential", 0o044; "/five", 0o065; "/florin", 0o246; "/four", 0o064;
  "/fraction", 0o244; "/gamma", 0o147; "/gradient", 0o321; "/greater", 0o076;
  "/greaterequal", 0o263; "/heart", 0o251; "/infinity", 0o245; "/integral",
  0o362; "/integraltp", 0o363; "/integralex", 0o364; "/integralbt", 0o365;
  "/intersection", 0o307; "/iota", 0o151; "/kappa", 0o153; "/lambda", 0o154;
  "/less", 0o074; "/lessequal", 0o243; "/logicaland", 0o331; "/logicalnot",
  0o330; "/logicalor", 0o332; "/lozenge", 0o340; "/minus", 0o055; "/minute",
  0o242; "/mu", 0o155; "/multiply", 0o264; "/nine", 0o071; "/notelement", 0o317;
  "/notequal", 0o271; "/notsubset", 0o313; "/nu", 0o156; "/numbersign", 0o043;
  "/omega", 0o167; "/omega1", 0o166; "/omicron", 0o157; "/one", 0o061;
  "/parenleft", 0o050; "/parenright", 0o051; "/parenlefttp", 0o346;
  "/parenleftex", 0o347; "/parenleftbt", 0o350; "/parenrighttp", 0o366;
  "/parenrightex", 0o367; "/parenrightbt", 0o370; "/partialdiff", 0o266;
  "/percent", 0o045; "/period", 0o056; "/perpendicular", 0o136; "/phi", 0o146;
  "/phi1", 0o152; "/pi", 0o160; "/plus", 0o153; "/plusminus", 0o261; "/product",
  0o325; "/propersubset", 0o314; "/propersuperset", 0o311; "/proportional",
  0o265; "/psi", 0o171; "/question", 0o077; "/radical", 0o326; "/radicalex",
  0o140; "/reflexsubset", 0o315; "/reflexsuperset", 0o312; "/registersans",
  0o342; "/registerserif", 0o322; "/rho", 0o162; "/second", 0o262; "/semicolon",
  0o073; "/seven", 0o067; "/sigma", 0o163; "/sigma1", 0o126; "/similar", 0o176;
  "/six", 0o066; "/slash", 0o157; "/space", 0o040; "/spade", 0o252; "/suchthat",
  0o047; "/summation", 0o345; "/tau", 0o164; "/therefore", 0o134; "/theta",
  0o161; "/theta1", 0o112; "/three", 0o063; "/trademarksans", 0o344;
  "/trademarkserif", 0o324; "/two", 0o062; "/underscore", 0o137; "/union",
  0o310; "/universal", 0o042; "/upsilon", 0o165; "/weierstrass", 0o303; "/xi",
  0o303; "/zero", 0o060; "/zeta", 0o172]

(* 6. Dingbats encoding *)
let name_to_dingbats = 
  ["/space", 0o040; "/a1", 0o041; "/a2", 0o042; "/a202", 0o043; "/a3", 0o044;
  "/a4", 0o045; "/a5", 0o046; "/a119", 0o047; "/a118", 0o050; "/a117", 0o051;
  "/a11", 0o052; "/a12", 0o053; "/a13", 0o054; "/a14", 0o055; "/a15", 0o056;
  "/a16", 0o057; "/a105", 0o060; "/a17", 0o061; "/a18", 0o062; "/a19", 0o063;
  "/a20", 0o064; "/a21", 0o065; "/a22", 0o066; "/a23", 0o067; "/a24", 0o070;
  "/a25", 0o071; "/a26", 0o072; "/a27", 0o073; "/a28", 0o074; "/a6", 0o075;
  "/a7", 0o076; "/a8", 0o077; "/a9", 0o100; "/a10", 0o101; "/a29", 0o102;
  "/a30", 0o103; "/a31", 0o104; "/a32", 0o105; "/a33", 0o106; "/a34", 0o107;
  "/a35", 0o110; "/a36", 0o111; "/a37", 0o112; "/a38", 0o113; "/a39", 0o114;
  "/a40", 0o115; "/a41", 0o116; "/a42", 0o117; "/a43", 0o120; "/a44", 0o121;
  "/a45", 0o122; "/a46", 0o123; "/a47", 0o124; "/a48", 0o125; "/a49", 0o126;
  "/a50", 0o127; "/a51", 0o130; "/a52", 0o131; "/a53", 0o132; "/a54", 0o133;
  "/a55", 0o134; "/a56", 0o135; "/a57", 0o136; "/a58", 0o137; "/a59", 0o140;
  "/a60", 0o141; "/a61", 0o142; "/a62", 0o143; "/a63", 0o144; "/a64", 0o145;
  "/a65", 0o146; "/a66", 0o147; "/a67", 0o150; "/a68", 0o151; "/a69", 0o152;
  "/a70", 0o153; "/a71", 0o154; "/a72", 0o155; "/a73", 0o156; "/a74", 0o157;
  "/a203", 0o160; "/a75", 0o161; "/a204", 0o162; "/a76", 0o163; "/a77", 0o164;
  "/a78", 0o165; "/a79", 0o166; "/a81", 0o167; "/a82", 0o170; "/a83", 0o171;
  "/a84", 0o172; "/a97", 0o173; "/a98", 0o174; "/a99", 0o175; "/a100", 0o176;
  "/a101", 0o241; "/a102", 0o242; "/a103", 0o243; "/a104", 0o244; "/a106",
  0o245; "/a107", 0o246; "/a108", 0o247; "/a112", 0o250; "/a111", 0o251;
  "/a110", 0o252; "/a109", 0o253; "/a120", 0o254; "/a121", 0o255; "/a122",
  0o256; "/a123", 0o257; "/a124", 0o260; "/a125", 0o261; "/a126", 0o262;
  "/a127", 0o263; "/a128", 0o264; "/a129", 0o265; "/a130", 0o266; "/a131",
  0o267; "/a132", 0o270; "/a133", 0o271; "/a134", 0o272; "/a135", 0o273;
  "/a136", 0o274; "/a137", 0o275; "/a138", 0o276; "/a139", 0o277; "/a140",
  0o300; "/a141", 0o301; "/a142", 0o302; "/a143", 0o303; "/a144", 0o304;
  "/a145", 0o305; "/a146", 0o306; "/a147", 0o307; "/a148", 0o310; "/a149",
  0o311; "/a150", 0o312; "/a151", 0o313; "/a152", 0o314; "/a153", 0O315;
  "/a154", 0o316; "/a155", 0o317; "/a156", 0o320; "/a157", 0o321; "/a158",
  0o322; "/a159", 0o323; "/a160", 0o324; "/a161", 0o325; "/a163", 0o326;
  "/a164", 0o327; "/a196", 0o330; "/a165", 0o331; "/a192", 0o332; "/a166",
  0o333; "/a167", 0o334; "/a168", 0o335; "/a169", 0o336; "/a170", 0o337;
  "/a171", 0o340; "/a172", 0o341; "/a173", 0o342; "/a162", 0o343; "/a174",
  0o344; "/a175", 0o345; "/a176", 0o346; "/a177", 0o347; "/a178", 0o350;
  "/a179", 0o351; "/a193", 0o352; "/a180", 0o353; "/a199", 0o354; "/a181",
  0o355; "/a200", 0o356; "/a182", 0o357; "/a201", 0o361; "/a183", 0o362;
  "/a184", 0o363; "/a197", 0o364; "/a185", 0o365; "/a194", 0o366; "/a198",
  0o367; "/a186", 0o370; "/a195", 0o371; "/a187", 0o372; "/a188", 0o373;
  "/a189", 0o374; "/a190", 0o375; "/a191", 0o376]

(* Unicode equivalents for some of the PDF ZapfDingbats Encoding (Heuristic!). *)
let dingbatmap_arr = 
[|"/a100", [0x275E]; "/a101", [0x2761]; "/a102", [0x2762]; "/a103", [0x2763];
"/a104", [0x2764]; "/a105", [0x2710]; "/a106", [0x2765]; "/a107", [0x2766];
"/a108", [0x2767]; "/a109", [0x2660]; "/a10", [0x2721]; "/a110", [0x2665];
"/a111", [0x2666]; "/a112", [0x2663]; "/a117", [0x2709]; "/a118", [0x2708];
"/a119", [0x2707]; "/a11", [0x261B]; "/a120", [0x2460]; "/a121", [0x2461];
"/a122", [0x2462]; "/a123", [0x2463]; "/a124", [0x2464]; "/a125", [0x2465];
"/a126", [0x2466]; "/a127", [0x2467]; "/a128", [0x2468]; "/a129", [0x2469];
"/a12", [0x261E]; "/a130", [0x2776]; "/a131", [0x2777]; "/a132", [0x2778];
"/a133", [0x2779]; "/a134", [0x277A]; "/a135", [0x277B]; "/a136", [0x277C];
"/a137", [0x277D]; "/a138", [0x277E]; "/a139", [0x277F]; "/a13", [0x270C];
"/a140", [0x2780]; "/a141", [0x2781]; "/a142", [0x2782]; "/a143", [0x2783];
"/a144", [0x2784]; "/a145", [0x2785]; "/a146", [0x2786]; "/a147", [0x2787];
"/a148", [0x2788]; "/a149", [0x2789]; "/a14", [0x270D]; "/a150", [0x278A];
"/a151", [0x278B]; "/a152", [0x278C]; "/a153", [0x278D]; "/a154", [0x278E];
"/a155", [0x278F]; "/a156", [0x2790]; "/a157", [0x2791]; "/a158", [0x2792];
"/a159", [0x2793]; "/a15", [0x270E]; "/a160", [0x2794]; "/a161", [0x2192];
"/a162", [0x27A3]; "/a163", [0x2194]; "/a164", [0x2195]; "/a165", [0x2799];
"/a166", [0x279B]; "/a167", [0x279C]; "/a168", [0x279D]; "/a169", [0x279E];
"/a16", [0x270F]; "/a170", [0x279F]; "/a171", [0x27A0]; "/a172", [0x27A1];
"/a173", [0x27A2]; "/a174", [0x27A4]; "/a175", [0x27A5]; "/a176", [0x27A6];
"/a177", [0x27A7]; "/a178", [0x27A8]; "/a179", [0x27A9]; "/a17", [0x2711];
"/a180", [0x27AB]; "/a181", [0x27AD]; "/a182", [0x27AF]; "/a183", [0x27B2];
"/a184", [0x27B3]; "/a185", [0x27B5]; "/a186", [0x27B8]; "/a187", [0x27BA];
"/a188", [0x27BB]; "/a189", [0x27BC]; "/a18", [0x2712]; "/a190", [0x27BD];
"/a191", [0x27BE]; "/a192", [0x279A]; "/a193", [0x27AA]; "/a194", [0x27B6];
"/a195", [0x27B9]; "/a196", [0x2798]; "/a197", [0x27B4]; "/a198", [0x27B7];
"/a199", [0x27AC]; "/a19", [0x2713]; "/a1", [0x2701]; "/a200", [0x27AE];
"/a201", [0x27B1]; "/a202", [0x2703]; "/a203", [0x2750]; "/a204", [0x2752];
"/a205", [0x276E]; "/a206", [0x2770]; "/a20", [0x2714]; "/a21", [0x2715];
"/a22", [0x2716]; "/a23", [0x2717]; "/a24", [0x2718]; "/a25", [0x2719];
"/a26", [0x271A]; "/a27", [0x271B]; "/a28", [0x271C]; "/a29", [0x2722];
"/a2", [0x2702]; "/a30", [0x2723]; "/a31", [0x2724]; "/a32", [0x2725];
"/a33", [0x2726]; "/a34", [0x2727]; "/a35", [0x2605]; "/a36", [0x2729];
"/a37", [0x272A]; "/a38", [0x272B]; "/a39", [0x272C]; "/a3", [0x2704];
"/a40", [0x272D]; "/a41", [0x272E]; "/a42", [0x272F]; "/a43", [0x2730];
"/a44", [0x2731]; "/a45", [0x2732]; "/a46", [0x2733]; "/a47", [0x2734];
"/a48", [0x2735]; "/a49", [0x2736]; "/a4", [0x260E]; "/a50", [0x2737];
"/a51", [0x2738]; "/a52", [0x2739]; "/a53", [0x273A]; "/a54", [0x273B];
"/a55", [0x273C]; "/a56", [0x273D]; "/a57", [0x273E]; "/a58", [0x273F];
"/a59", [0x2740]; "/a5", [0x2706]; "/a60", [0x2741]; "/a61", [0x2742];
"/a62", [0x2743]; "/a63", [0x2744]; "/a64", [0x2745]; "/a65", [0x2746];
"/a66", [0x2747]; "/a67", [0x2748]; "/a68", [0x2749]; "/a69", [0x274A];
"/a6", [0x271D]; "/a70", [0x274B]; "/a71", [0x25CF]; "/a72", [0x274D];
"/a73", [0x25A0]; "/a74", [0x274F]; "/a75", [0x2751]; "/a76", [0x25B2];
"/a77", [0x25BC]; "/a78", [0x25C6]; "/a79", [0x2756]; "/a7", [0x271E];
"/a81", [0x25D7]; "/a82", [0x2758]; "/a83", [0x2759]; "/a84", [0x275A];
"/a85", [0x276F]; "/a86", [0x2771]; "/a87", [0x2772]; "/a88", [0x2773];
"/a89", [0x2768]; "/a8", [0x271F]; "/a90", [0x2769]; "/a91", [0x276C];
"/a92", [0x276D]; "/a93", [0x276A]; "/a94", [0x276B]; "/a95", [0x2774];
"/a96", [0x2775]; "/a97", [0x275B]; "/a98", [0x275C]; "/a99", [0x275D];
"/a9", [0x2720]|]

let dingbatmap = Array.to_list dingbatmap_arr

(* Ditto truetype. Heuristic! *)
let truetypemap_arr =
[|"/G20", [0x0020]; "/G21", [0x0021]; "/G22", [0x0022]; "/G23", [0x0023];
"/G24", [0x0024]; "/G25", [0x0025]; "/G26", [0x0026]; "/G27", [0x0027];
"/G28", [0x0028]; "/G29", [0x0029]; "/G2a", [0x002A]; "/G2b", [0x002B];
"/G2c", [0x002C]; "/G2d", [0x002D]; "/G2e", [0x002E]; "/G2f", [0x002F];
"/G30", [0x0030]; "/G31", [0x0031]; "/G32", [0x0032]; "/G33", [0x0033];
"/G34", [0x0034]; "/G35", [0x0035]; "/G36", [0x0036]; "/G37", [0x0037];
"/G38", [0x0038]; "/G39", [0x0039]; "/G3a", [0x003A]; "/G3b", [0x003B];
"/G3c", [0x003C]; "/G3d", [0x003D]; "/G3e", [0x003E]; "/G3f", [0x003F];
"/G40", [0x0040]; "/G41", [0x0041]; "/G42", [0x0042]; "/G43", [0x0043];
"/G44", [0x0044]; "/G45", [0x0045]; "/G46", [0x0046]; "/G47", [0x0047];
"/G48", [0x0048]; "/G49", [0x0049]; "/G4a", [0x004A]; "/G4b", [0x004B];
"/G4c", [0x004C]; "/G4d", [0x004D]; "/G4e", [0x004E]; "/G4f", [0x004F];
"/G50", [0x0050]; "/G51", [0x0051]; "/G52", [0x0052]; "/G53", [0x0053];
"/G54", [0x0054]; "/G55", [0x0055]; "/G56", [0x0056]; "/G57", [0x0057];
"/G58", [0x0058]; "/G59", [0x0059]; "/G5a", [0x005A]; "/G5b", [0x005B];
"/G5c", [0x005C]; "/G5d", [0x005D]; "/G5e", [0x005E]; "/G5f", [0x005F];
"/G60", [0x0060]; "/G61", [0x0061]; "/G62", [0x0062]; "/G63", [0x0063];
"/G64", [0x0064]; "/G65", [0x0065]; "/G66", [0x0066]; "/G67", [0x0067];
"/G68", [0x0068]; "/G69", [0x0069]; "/G6a", [0x006A]; "/G6b", [0x006B];
"/G6c", [0x006C]; "/G6d", [0x006D]; "/G6e", [0x006E]; "/G6f", [0x006F];
"/G70", [0x0070]; "/G71", [0x0071]; "/G72", [0x0072]; "/G73", [0x0073];
"/G74", [0x0074]; "/G75", [0x0075]; "/G76", [0x0076]; "/G77", [0x0077];
"/G78", [0x0078]; "/G79", [0x0079]; "/G7a", [0x007A]; "/G7b", [0x007B];
"/G7c", [0x007C]; "/G7d", [0x007D]; "/G7e", [0x007E]; "/Ga0", [0x00A0];
"/Ga1", [0x00A1]; "/Ga2", [0x00A2]; "/Ga3", [0x00A3]; "/Ga4", [0x00A4];
"/Ga5", [0x00A5]; "/Ga6", [0x00A6]; "/Ga7", [0x00A7]; "/Ga8", [0x00A8];
"/Ga9", [0x00A9]; "/Gaa", [0x00AA]; "/Gab", [0x00AB]; "/Gac", [0x00AC];
"/Gad", [0x00AD]; "/Gae", [0x00AE]; "/Gaf", [0x00AF]; "/Gb0", [0x00B0];
"/Gb1", [0x00B1]; "/Gb2", [0x00B2]; "/Gb3", [0x00B3]; "/Gb4", [0x00B4];
"/Gb5", [0x00B5]; "/Gb6", [0x00B6]; "/Gb7", [0x00B7]; "/Gb8", [0x00B8];
"/Gb9", [0x00B9]; "/Gba", [0x00BA]; "/Gbb", [0x00BB]; "/Gbc", [0x00BC];
"/Gbd", [0x00BD]; "/Gbe", [0x00BE]; "/Gbf", [0x00BF]; "/Gc0", [0x00C0];
"/Gc1", [0x00C1]; "/Gc2", [0x00C2]; "/Gc3", [0x00C3]; "/Gc4", [0x00C4];
"/Gc5", [0x00C5]; "/Gc6", [0x00C6]; "/Gc7", [0x00C7]; "/Gc8", [0x00C8];
"/Gc9", [0x00C9]; "/Gca", [0x00CA]; "/Gcb", [0x00CB]; "/Gcc", [0x00CC];
"/Gcd", [0x00CD]; "/Gce", [0x00CE]; "/Gcf", [0x00CF]; "/Gd0", [0x00D0];
"/Gd1", [0x00D1]; "/Gd2", [0x00D2]; "/Gd3", [0x00D3]; "/Gd4", [0x00D4];
"/Gd5", [0x00D5]; "/Gd6", [0x00D6]; "/Gd7", [0x00D7]; "/Gd8", [0x00D8];
"/Gd9", [0x00D9]; "/Gda", [0x00DA]; "/Gdb", [0x00DB]; "/Gdc", [0x00DC];
"/Gdd", [0x00DD]; "/Gde", [0x00DE]; "/Gdf", [0x00DF]; "/Ge0", [0x00E0];
"/Ge1", [0x00E1]; "/Ge2", [0x00E2]; "/Ge3", [0x00E3]; "/Ge4", [0x00E4];
"/Ge5", [0x00E5]; "/Ge6", [0x00E6]; "/Ge7", [0x00E7]; "/Ge8", [0x00E8];
"/Ge9", [0x00E9]; "/Gea", [0x00EA]; "/Geb", [0x00EB]; "/Gec", [0x00EC];
"/Ged", [0x00ED]; "/Gee", [0x00EE]; "/Gef", [0x00EF]; "/Gf0", [0x00F0];
"/Gf1", [0x00F1]; "/Gf2", [0x00F2]; "/Gf3", [0x00F3]; "/Gf4", [0x00F4];
"/Gf5", [0x00F5]; "/Gf6", [0x00F6]; "/Gf7", [0x00F7]; "/Gf8", [0x00F8];
"/Gf9", [0x00F9]; "/Gfa", [0x00FA]; "/Gfb", [0x00FB]; "/Gfc", [0x00FC];
"/Gfd", [0x00FD]; "/Gfe", [0x00FE]; "/Gff", [0x00FF]; "/G82", [0x201A];
"/G83", [0x0192]; "/G84", [0x201E]; "/G85", [0x2026]; "/G86", [0x2020];
"/G87", [0x2021]; "/G88", [0x02C6]; "/G89", [0x2030]; "/G8a", [0x0160];
"/G8b", [0x2039]; "/G8c", [0x0152]; "/G91", [0x2018]; "/G92", [0x2019];
"/G93", [0x201C]; "/G94", [0x201D]; "/G95", [0x2022]; "/G96", [0x2013];
"/G97", [0x2014]; "/G98", [0x02DC]; "/G99", [0x2122]; "/G9a", [0x0161];
"/G9b", [0x203A]; "/G9c", [0x0153]; "/G9f", [0x0178]; "/G2A", [0x002A];
"/G2B", [0x002B]; "/G2C", [0x002C]; "/G2D", [0x002D]; "/G2E", [0x002E];
"/G2F", [0x002F]; "/G3A", [0x003A]; "/G3B", [0x003B]; "/G3C", [0x003C];
"/G3D", [0x003D]; "/G3E", [0x003E]; "/G3F", [0x003F]; "/G4A", [0x004A];
"/G4B", [0x004B]; "/G4C", [0x004C]; "/G4D", [0x004D]; "/G4E", [0x004E];
"/G4F", [0x004F]; "/G5A", [0x005A]; "/G5B", [0x005B]; "/G5C", [0x005C];
"/G5D", [0x005D]; "/G5E", [0x005E]; "/G5F", [0x005F]; "/G6A", [0x006A];
"/G6B", [0x006B]; "/G6C", [0x006C]; "/G6D", [0x002D]; "/G6E", [0x006E];
"/G6F", [0x006F]; "/G7A", [0x007A]; "/G7B", [0x007B]; "/G7C", [0x007C];
"/G7D", [0x007D]; "/G7E", [0x007E]; "/G8A", [0x008A]; "/G8B", [0x008B];
"/G8C", [0x008C]; "/G9A", [0x009A]; "/G9B", [0x009B]; "/G9C", [0x009C];
"/GA0", [0x00A0]; "/GA1", [0x00A1]; "/GA2", [0x00A2]; "/GA3", [0x00A3];
"/GA4", [0x00A4]; "/GA5", [0x00A5]; "/GA6", [0x00A6]; "/GA7", [0x00A7];
"/GA8", [0x00A8]; "/GA9", [0x00A9]; "/GAA", [0x00AA]; "/GAB", [0x00AB];
"/GAC", [0x00AC]; "/GAD", [0x00AD]; "/GAE", [0x00AE]; "/GAF", [0x00AF];
"/GB0", [0x00B0]; "/GB1", [0x00B1]; "/GB2", [0x00B2]; "/GB3", [0x00B3];
"/GB4", [0x00B4]; "/GB5", [0x00B5]; "/GA6", [0x00B6]; "/GB7", [0x00B7];
"/GB8", [0x00B8]; "/GB9", [0x00B9]; "/GBA", [0x00BA]; "/GBB", [0x00BB];
"/GBC", [0x00BC]; "/GBD", [0x00BD]; "/GBE", [0x00BE]; "/GBF", [0x00BF];
"/GC0", [0x00C0]; "/GC1", [0x00C1]; "/GC2", [0x00C2]; "/GC3", [0x00C3];
"/GC4", [0x00C4]; "/GC5", [0x00C5]; "/GC6", [0x00C6]; "/GC7", [0x00C7];
"/GC8", [0x00C8]; "/GC9", [0x00C9]; "/GCA", [0x00CA]; "/GCB", [0x00CB];
"/GCC", [0x00CC]; "/GCD", [0x00CD]; "/GCE", [0x00CE]; "/GCF", [0x00CF];
"/GD0", [0x00D0]; "/GD1", [0x00D1]; "/GD2", [0x00D2]; "/GD3", [0x00D3];
"/GD4", [0x00D4]; "/GD5", [0x00D5]; "/GD6", [0x00D6]; "/GD7", [0x00D7];
"/GD8", [0x00D8]; "/GD9", [0x00D9]; "/GDA", [0x00DA]; "/GDB", [0x00DB];
"/GDC", [0x00DC]; "/GDD", [0x00DD]; "/GDE", [0x00DE]; "/GDF", [0x00DF];
"/GE0", [0x00E0]; "/GE1", [0x00E1]; "/GE2", [0x00E2]; "/GE3", [0x00E3];
"/GE4", [0x00E4]; "/GE5", [0x00E5]; "/GE6", [0x00E6]; "/GE7", [0x00E7];
"/GE8", [0x00E8]; "/GE9", [0x00E9]; "/GEA", [0x00EA]; "/GEB", [0x00EB];
"/GEC", [0x00EC]; "/GED", [0x00ED]; "/GEE", [0x00EE]; "/GEF", [0x00EF];
"/GF0", [0x00F0]; "/GF1", [0x00F1]; "/GF2", [0x00F2]; "/GF3", [0x00F3];
"/GF4", [0x00F4]; "/GF5", [0x00F5]; "/GF6", [0x00F6]; "/GF7", [0x00F7];
"/GF8", [0x00F8]; "/GF9", [0x00F9]; "/GFA", [0x00FA]; "/GFB", [0x00FB];
"/GFC", [0x00FC]; "/GFD", [0x00FD]; "/GFE", [0x00FE]; "/GFF", [0x00FF]|]

let truetypemap = Array.to_list truetypemap_arr

(* The reverse glyph map is not a 1-1 mapping. So we need special version of
hashtable_of_dictionary which a) prefers glyphs with alphabetic names over
those earlier in the map which have numeric names b) Doesn't overwrite any
entry already there, preserving any intelligence which might be in the ordering
of the glyph map *)
let revglyph_hashtable_of_dictionary pairs =
  let contains_digit s =
    mem true (map isdigit (explode s))
  in
    let table = Hashtbl.create (length pairs) in
      iter
        (fun (k, v) ->
           try
             if contains_digit (Hashtbl.find table k) && not (contains_digit v)
               then Hashtbl.replace table k v
           with
             Not_found -> Hashtbl.add table k v)
        pairs;
      table

let glyph_hashes =
  memoize (fun () -> hashtable_of_dictionary (glyphmap () @ dingbatmap @ truetypemap))

let reverse_glyph_hashes =
  memoize (fun () -> revglyph_hashtable_of_dictionary (map (fun (a, b) -> (b, a)) (glyphmap ())))

let name_to_pdf_hashes =
  hashtable_of_dictionary name_to_pdf

let reverse_name_to_pdf_hashes =
  hashtable_of_dictionary (map (fun (a, b) -> (b, a)) name_to_pdf)
