namespace SRTV

module Regex =
    open System
    open System.Globalization
    open System.Text.RegularExpressions
    open System.Text.Unicode

    module Urls =

        let [<Literal>] TCOSlugMaxLength = 40
        let [<Literal>] MaxURLLength = 4096

        let toRange (range:UnicodeRange) = 
            let lastCodePoint = range.FirstCodePoint + range.Length - 1
            sprintf @"\u%04x-\u%04x" range.FirstCodePoint lastCodePoint

        let [<Literal>] ValidURLPortNumber = @"(?>\d+)"

        let [<Literal>] ValidURLQueryCharacters = @"[a-z0-9!?\*'\(\);:&=\+\$/%#\[\]\-_\.,~\|@]"
        let [<Literal>] ValidURLQueryEndingCharacters = @"[a-z0-9\-_&=#/]"

        let [<Literal>] private ``Latin-1`` = @"\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u00ff"
        let [<Literal>] private IPAExtensions = @"\u0253\u0254\u0256\u0257\u0259\u025b\u0263\u0268\u026f\u0272\u0289\u028b"
        let [<Literal>] private Hawaiian = @"\u02bb"

        let private Latin = 
            ``Latin-1``
            + toRange UnicodeRanges.LatinExtendedA
            + toRange UnicodeRanges.LatinExtendedB
            + IPAExtensions
            + Hawaiian
            + toRange UnicodeRanges.CombiningDiacriticalMarks
            + toRange UnicodeRanges.LatinExtendedAdditional
        
        let [<Literal>] private PunycodeURL = @"(?:xn--[-0-9a-z]+)"
        let private ValidURLCharacters = "[a-z0-9" + Latin + "]"

        let [<Literal>] private InvalidCharacters = @"\uFFFE\uFEFF\uFFFF"
        let [<Literal>] private DirectionalCharacters = @"\u061C\u200E\u200F\u202A\u202B\u202C\u202D\u202E\u2066\u2067\u2068\u2069"

        let [<Literal>] private ValidURLPrecedingCharacters = @"(?:[^a-z0-9@＠$#＃" + InvalidCharacters + "]|[" + DirectionalCharacters + "]|^)"

        let private ValidURLGeneralPathCharacters =
            $@"[a-z0-9!\*';:=\+,.\$/%%#\[\]\-\u2013_~\|&@%s{Latin}%s{toRange UnicodeRanges.Cyrillic}]"

        let private UrlBalancedParens = 
            $@"\((?:{ValidURLGeneralPathCharacters}+|(?:{ValidURLGeneralPathCharacters}*\({ValidURLGeneralPathCharacters}+\){ValidURLGeneralPathCharacters}*))\)"

        let private ValidURLPathEndingCharacters = 
            $@"[a-z0-9=_#/\-\+%s{Latin}%s{toRange UnicodeRanges.Cyrillic}]|(?:{UrlBalancedParens})"

        let private ValidURLPath =
            $@"(?:(?:{ValidURLGeneralPathCharacters}*(?:{UrlBalancedParens}{ValidURLGeneralPathCharacters}*)*{ValidURLPathEndingCharacters})|(?:@{ValidURLGeneralPathCharacters}+/))"
            
        let private ValidURLSubdomain = 
            $@"(?>(?:{ValidURLCharacters}(?:{ValidURLCharacters}|[\-_])*)?{ValidURLCharacters}\.)"

        let private ValidURLDomainName =
            $@"(?:(?:{ValidURLCharacters}(?:{ValidURLCharacters}|-)*)?{ValidURLCharacters}\.)"

        let [<Literal>] private ValidURLGTLD = 
            "(?:(?:삼성|닷컴|닷넷|香格里拉|餐厅|食品|飞利浦|電訊盈科|集团|通販|购物|谷歌|诺基亚|联通|网络|网站|网店|网址|组织机构|移动|珠宝\|点看|游戏|淡马锡|机构|書籍|时尚|新闻|政府|政务|招聘|手表|手机|我爱你|慈善|微博|广东|工行|家電|娱乐|天主教|大拿|大众汽车|在线|嘉里大酒店|嘉里|商标|商店|商城|公益|公司|八卦|健康|信息|佛山|企业|中文网|中信|世界|ポイント|ファッション|セール|ストア|コム|グ
            ーグル|クラウド|みんな|คอม|संगठन|नेट|कॉम|همراه|موقع|موبايلي|كوم|كاثوليك|عرب|شبكة|بيتك|بازار|العليان|ارامكو|اتصالات|ابوظبي|קום|
            сайт|рус|орг|онлайн|москва|ком|католик|дети|zuerich|zone|zippo|zip|zero|zara|zappos|yun|youtube|you|yokohama|yoga|yodobashi|yandex|yamaxun|yahoo|yachts|xyz|xxx\
            |xperia|xin|xihuan|xfinity|xerox|xbox|wtf|wtc|wow|world|works|work|woodside\
            |wolterskluwer|wme|winners|wine|windows|win|williamhill|wiki|wien|whoswho|weir|weibo|wedding|wed|website|weber|webcam|weatherchannel|weather|watches|watch|warman|wanggou|wang|walter|walmart|wales|vuelos|voyage|voto|voting|vote|volvo|volkswagen|vodka|vlaanderen|vivo|viva|vistaprint\
            |vista|vision|visa|virgin|vip|vin|villas|viking|vig|video|viajes\
            |vet|versicherung|vermögensberatung|vermögensberater|verisign|ventures\
            |vegas|vanguard|vana|vacations|ups|uol|uno|university|unicom|uconnect|ubs|ubank|tvs|tushu|tunes|tui|tube|trv|trust|travelersinsurance|travelers|travelchannel|travel|training|trading|trade|toys|toyota|town|tours|total|toshiba|toray|top|tools|tokyo|today|tmall|tkmaxx|tjx|tjmaxx|tirol|tires|tips|tiffany|tienda|tickets|tiaa|theatre|theater|thd|teva|tennis|temasek|telefonica|telecity|tel|technology|tech|team|tdk|tci|taxi|tax|tattoo|tatar|tatamotors|target|taobao|talk|taipei|tab|systems|symantec|sydney|swiss|swiftcover|swatch|suzuki|surgery|surf|support|supply|supplies|sucks|style|study|studio|stream|store|storage|stockholm|stcgroup|stc|statoil|statefarm|statebank|starhub|star|staples|stada|srt|srl|spreadbetting|spot|sport|spiegel|space|soy|sony|song|solutions|solar|sohu|software|softbank|social|soccer|sncf|smile|smart|sling|skype|sky|skin|ski|site|singles|sina|silk|shriram|showtime|show|shouji|shopping|shop|shoes|shiksha|shia|shell|shaw|sharp|shangrila|sfr|sexy|sex|sew|seven|ses|services|sener|select|seek|security|secure|seat|search|scot|scor|scjohnson|science|schwarz|schule|school|scholarships|schmidt|schaeffler|scb|sca|sbs|sbi|saxo|save|sas|sarl|sapo|sap|sanofi|sandvikcoromant|sandvik|samsung|samsclub|salon|sale|sakura|safety|safe|saarland|ryukyu|rwe|run|ruhr|rugby|rsvp|room|rogers|rodeo|rocks|rocher|rmit|rip|rio|ril|rightathome|ricoh|richardli|rich|rexroth|reviews|review|restaurant|rest|republican|report|repair|rentals|rent|ren|reliance|reit|reisen|reise|rehab|redumbrella|redstone|red|recipes|realty|realtor|realestate|read|raid|radio|racing|qvc|quest|quebec|qpon|pwc|pub|prudential|pru|protection|property|properties|promo|progressive|prof|productions|prod|pro|prime|press|praxi|pramerica|post|porn|politie|poker|pohl|pnc|plus|plumbing|playstation|play|place|pizza|pioneer|pink|ping|pin|pid|pictures|pictet|pics|piaget|physio|photos|photography|photo|phone|philips|phd|pharmacy|pfizer|pet|pccw|pay|passagens|party|parts|partners|pars|paris|panerai|panasonic|pamperedchef|page|ovh|ott|otsuka|osaka|origins|orientexpress|organic|org|orange|oracle|open|ooo|onyourside|online|onl|ong|one|omega|ollo|oldnavy|olayangroup|olayan|okinawa|office|off|observer|obi|nyc|ntt|nrw|nra|nowtv|nowruz|now|norton|northwesternmutual|nokia|nissay|nissan|ninja|nikon|nike|nico|nhk|ngo|nfl|nexus|nextdirect|next|news|newholland|new|neustar|network|netflix|netbank|net|nec|nba|navy|natura|nationwide|name|nagoya|nadex|nab|mutuelle|mutual|museum|mtr|mtpc|mtn|msd|movistar|movie|mov|motorcycles|moto|moscow|mortgage|mormon|mopar|montblanc|monster|money|monash|mom|moi|moe|moda|mobily|mobile|mobi|mma|mls|mlb|mitsubishi|mit|mint|mini|mil|microsoft|miami|metlife|merckmsd|meo|menu|men|memorial|meme|melbourne|meet|media|med|mckinsey|mcdonalds|mcd|mba|mattel|maserati|marshalls|marriott|markets|marketing|market|map|mango|management|man|makeup|maison|maif|madrid|macys|luxury|luxe|lupin|lundbeck|ltda|ltd|lplfinancial|lpl|love|lotto|lotte|london|lol|loft|locus|locker|loans|loan|llp|llc|lixil|living|live|lipsy|link|linde|lincoln|limo|limited|lilly|like|lighting|lifestyle|lifeinsurance|life|lidl|liaison|lgbt|lexus|lego|legal|lefrak|leclerc|lease|lds|lawyer|law|latrobe|latino|lat|lasalle|lanxess|landrover|land|lancome|lancia|lancaster|lamer|lamborghini|ladbrokes|lacaixa|kyoto|kuokgroup|kred|krd|kpn|kpmg|kosher|komatsu|koeln|kiwi|kitchen|kindle|kinder|kim|kia|kfh|kerryproperties|kerrylogistics|kerryhotels|kddi|kaufen|juniper|juegos|jprs|jpmorgan|joy|jot|joburg|jobs|jnj|jmp|jll|jlc|jio|jewelry|jetzt|jeep|jcp|jcb|java\
            |jaguar|iwc|iveco|itv|itau|istanbul|ist|ismaili|iselect|irish|ipiranga|investments|intuit|international|intel|int|insure|insurance|institute|ink|ing|info|infiniti|industries|inc|immobilien|immo|imdb|imamat|ikano|iinet|ifm|ieee|icu|ice|icbc|ibm|hyundai|hyatt|hughes|htc|hsbc|how|house|hotmail|hotels|hoteles|hot|hosting|host|hospital|horse|honeywell|honda|homesense|homes|homegoods|homedepot|holiday|holdings|hockey|hkt|hiv|hitachi|hisamitsu|hiphop|hgtv|hermes|here|helsinki|help|healthcare|health|hdfcbank|hdfc|hbo|haus|hangout|hamburg|hair|guru|guitars|guide|guge|gucci|guardian|group|grocery|gripe|green|gratis|graphics|grainger|gov|got|gop|google|goog|goodyear|goodhands|goo|golf|goldpoint|gold|godaddy|gmx|gmo|gmbh|gmail|globo|global|gle|glass|glade|giving|gives|gifts|gift|ggee|george|genting|gent|gea|gdn|gbiz|gay|garden|gap|games|game|gallup|gallo|gallery|gal|fyi|futbol|furniture|fund|fun|fujixerox|fujitsu|ftr|frontier|frontdoor|frogans|frl|fresenius|free|fox|foundation|forum|forsale|forex|ford|football|foodnetwork|food|foo|fly|flsmidth|flowers|florist|flir|flights|flickr|fitness|fit|fishing|fish|firmdale|firestone|fire|financial|finance|final|film|fido|fidelity|fiat|ferrero|ferrari|feedback|fedex|fast|fashion|farmers|farm|fans|fan|family|faith|fairwinds|fail|fage|extraspace|express|exposed|expert|exchange|everbank|events|eus|eurovision|etisalat|esurance|estate|esq|erni|ericsson|equipment|epson|epost|enterprises|engineering|engineer|energy|emerck|email|education|edu|edeka|eco\
            |eat|earth|dvr|dvag|durban|dupont|duns|dunlop|duck|dubai|dtv|drive|download|dot|doosan|domains|doha|dog|dodge|doctor|docs|dnp|diy|dish|discover|discount|directory|direct|digital|diet|diamonds|dhl|dev|design|desi|dentist|dental|democrat|delta|deloitte|dell|delivery|degree|deals|dealer|deal|dds|dclk|day|datsun|dating|date|data|dance|dad|dabur|cyou|cymru|cuisinella|csc|cruises|cruise|crs|crown|cricket|creditunion|creditcard|credit|cpa|courses|coupons|coupon|country|corsica|coop|cool|cookingchannel\
            |cooking|contractors|contact|consulting|construction|condos|comsec|computer|compare|company|community|commbank|comcast|com|cologne|college|coffee|codes|coach|clubmed|club|cloud|clothing|clinique|clinic|click|cleaning|claims|cityeats|city|citic|citi|citadel|cisco|circle|cipriani|church|chrysler|chrome|christmas|chloe|chintai|cheap|chat|chase|charity|channel|chanel|cfd|cfa|cern|ceo|center|ceb|cbs|cbre|cbn|cba|catholic|catering|cat|casino|cash|caseih|case|casa|cartier|cars|careers|career|care|cards|caravan|car|capitalone|capital|capetown|canon|cancerresearch|camp|camera|cam|calvinklein|call|cal|cafe|cab|bzh|buzz|buy|business|builders|build|bugatti|budapest|brussels|brother|broker|broadway|bridgestone|bradesco|box|boutique|bot|boston|bostik|bosch|boots|booking|book|boo|bond|bom|bofa|boehringer|boats|bnpparibas|bnl|bmw|bms|blue|bloomberg|blog|blockbuster|blanco|blackfriday|black|biz|bio|bingo|bing|bike|bid|bible|bharti|bet|bestbuy|best|berlin|bentley|beer|beauty|beats|bcn|bcg|bbva|bbt|bbc|bayern|bauhaus|basketball|baseball|bargains|barefoot|barclays|barclaycard|barcelona|bar|bank|band|bananarepublic|banamex|baidu|baby|azure|axa|aws|avianca|autos|auto|author|auspost|audio|audible|audi|auction|attorney|athleta|associates|asia|asda|arte|art|arpa|army|archi|aramco|arab|aquarelle|apple|app|apartments|aol|anz|anquan|android|analytics|amsterdam|amica|amfam|amex|americanfamily|americanexpress|alstom|alsace|ally|allstate|allfinanz|alipay|alibaba|alfaromeo|akdn|airtel|airforce|airbus|aigo|aig|agency|agakhan|africa|afl|afamilycompany|aetna|aero|aeg|adult|ads|adac|actor|active|aco|accountants|accountant|accenture|academy|abudhabi|abogado|able|abc|abbvie|abbott|abb|abarth|aarp|aaa|onion)(?=[^a-z0-9@+-]|$))"

        let [<Literal>] private ValidURLCCTLD = 
            "(?:(?:한국|香港|澳門|新加坡|台灣|台湾|中國|中国|გე|ລາວ|ไทย|ලංකා|ഭാരതം|ಭಾರತ|భారత్\
            |சிங்கப்பூர்|இலங்கை|இந்தியா|ଭାରତ|ભારત|ਭਾਰਤ|ভাৰত|ভারত|বাংলা|भारोत|भारतम्|भारत\
            |ڀارت|پاکستان|موريتانيا|مليسيا|مصر\
            |قطر|فلسطين|عمان|عراق|سورية|سودان|تونس|بھارت|\
            بارت|ایران|امارات|المغرب|السعودية|الجزائر|البحرين|الاردن\
            |հայ|қаз|укр|срб|рф|мон|мкд|ею|бел|бг|ευ|ελ|zw|zm|za|yt|ye|ws\
            |wf|vu|vn|vi|vg|ve|vc|va|uz|uy|us|um|uk|ug|ua|tz|tw|tv|tt|tr|tp\
            |to|tn|tm|tl|tk|tj|th|tg|tf|td|tc|sz|sy|sx|sv|su|st|ss|sr|so|sn|sm\
            |sl|sk|sj|si|sh|sg|se|sd|sc|sb|sa|rw|ru|rs|ro|re|qa|py|pw|pt|ps|pr\
            |pn|pm|pl|pk|ph|pg|pf|pe|pa|om|nz|nu|nr|np|no|nl|ni|ng|nf|ne|nc|na\
            |mz|my|mx|mw|mv|mu|mt|ms|mr|mq|mp|mo|mn|mm|ml|mk|mh|mg|mf|me|md|mc\
            |ma|ly|lv|lu|lt|ls|lr|lk|li|lc|lb|la|kz|ky|kw|kr|kp|kn|km|ki|kh|kg|ke|jp|jo|jm|je\
            |it|is|ir|iq|io|in|im|il|ie|id|hu|ht|hr|hn|hm|hk|gy|gw|gu|gt|gs|gr|gq|gp|gn\
            |gm|gl|gi|gh|gg|gf|ge|gd|gb|ga|fr|fo|fm|fk|fj|fi|eu|et|es|er|eh|eg|ee|ec|dz\
            |do|dm|dk|dj|de|cz|cy|cx|cw|cv|cu|cr|co|cn|cm|cl|ck|ci|ch|cg|cf|cd|cc|ca|bz|by|bw|bv\
            |bt|bs|br|bq|bo|bn|bm|bl|bj|bi|bh|bg|bf|be|bd|bb|ba|az|ax|aw|au|at|as|ar|aq\
            |ao|an|am|al|ai|ag|af|ae|ad|ac)(?=[^a-z0-9@+-]|$))"

        let [<Literal>] private PunctuationCharacters =
            @"-_!\""#$%&'\(\)*+,./:;<=>?@\[\]^`\{|}~"

        let private ValidURLUnicodeCharacters =
            $@"[^{PunctuationCharacters}\s\p{{Z}}\p{{IsGeneralPunctuation}}]"

        let private ValidURLUnicodeDomainName = 
            $@"(?:(?:{ValidURLUnicodeCharacters}(?:{ValidURLUnicodeCharacters}|-)*)?{ValidURLUnicodeCharacters}\.)"

        let private ValidURLDomain =
            $"(?:{ValidURLSubdomain}*{ValidURLDomainName}(?:{ValidURLGTLD}|{ValidURLCCTLD}|{PunycodeURL}))|\
            (?:(?<=https?://)(?:(?:{ValidURLDomainName}{ValidURLCCTLD})|\
            (?:{ValidURLUnicodeDomainName}(?:{ValidURLGTLD}|{ValidURLCCTLD}))))|(?:{ValidURLDomainName}{ValidURLCCTLD}(?=/))"

        let private ValidURLPattern =
            $"(?<all>\
                (?<preceding>{ValidURLPrecedingCharacters})\
                (?<url>\
                    (?<protocol>https?://)?\
                    (?<domain>{ValidURLDomain})(?::\
                    (?<port>{ValidURLPortNumber}))?\
                    (?<pathAnchor>/(?>{ValidURLPath}*))?\
                    (?<query>\\?{ValidURLQueryCharacters}*{ValidURLQueryEndingCharacters})?))"

        let private ValidTCOUrl =
            $@"^https?://t\.co/([a-z0-9]+)(?:\?{ValidURLQueryCharacters}*{ValidURLQueryEndingCharacters})?"

        let private urlGroupProtocolLength = String.length "https://"

        let private validURL = new Regex(ValidURLPattern, RegexOptions.IgnoreCase)
        let private tcoURL = new Regex(ValidTCOUrl, RegexOptions.IgnoreCase)

        type TweetURL = {
            url: string
            start: int
        }

        let private validHostWithValidLength urlLength (protocol:string) (host:string) =
            Some host
            |> Option.filter (String.IsNullOrEmpty >> not)
            |> Option.map (fun host -> 
                let idn = new IdnMapping()
                idn.AllowUnassigned <- true
                try
                    idn.GetAscii(host)
                with
                | :? ArgumentException | :? ArgumentNullException -> ""
            )
            |> Option.filter (String.IsNullOrEmpty >> not)
            |> Option.map (fun idnHost -> host.Length + idnHost.Length+ urlLength)
            |> Option.map (fun newUrlLength -> newUrlLength + (if protocol.Length = 0 then urlGroupProtocolLength else 0))
            |> Option.filter (fun length -> length <= MaxURLLength)
            |> Option.isSome
                    

        let extractUrls text =
            if String.forall (fun x -> x <> '.') text then []
            else

                let matches = validURL.Matches(text)
                printfn "Placeholder"

                validURL.Matches(text)
                |> Seq.cast<Match>
                |> Seq.map (fun m -> m.Groups)
                |> Seq.filter (fun groups -> groups.["protocol"].Success || not (Regex.IsMatch(groups.["preceding"].Value, "[-_./]$")))
                |> Seq.map (fun groups -> 
                    let url = groups.["url"].Value
                    let tcoMatch = tcoURL.Match(url)
                    {|  url = if tcoMatch.Success then tcoMatch.Groups.[0].Value else url
                        start = groups.["url"].Index
                        protocol = groups.["protocol"].Value
                        host = groups.["domain"].Value
                    |})
                |> Seq.filter (fun x ->
                    let tcoMatch = tcoURL.Match(x.url)
                    (not tcoMatch.Success) || (tcoMatch.Groups.[1].Value.Length <= TCOSlugMaxLength))
                |> Seq.filter (fun x -> validHostWithValidLength x.url.Length x.protocol x.host)
                |> Seq.map (fun x -> { url = x.url; start = x.start })
                |> Seq.toList