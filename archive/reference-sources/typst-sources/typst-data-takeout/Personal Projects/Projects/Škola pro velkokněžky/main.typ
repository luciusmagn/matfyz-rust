#import "@preview/touying:0.5.3": *
#import themes.simple: *
#show: simple-theme.with(
 aspect-ratio: "16-9",
 footer: self => "",
 config-info(
   title: [Knížecí akademie digitálních médií],
   author: [Lukáš Hozda],
   date: datetime.today(),
   institution: [Braiins Systems s.r.o],
 ),
)
#set page(margin: (x: 0pt, y: 0pt), header: none)
#show raw: set text(size: 0.7em)

#title-slide()[
  #align(center)[
  = Vzdělávání Velkokněžky
  Knížecí akademie digitálních médií
  ]

  léta Páně 2025
]

== Na programu
- Něco z teorie
- Color correction a color grading
- Formáty fotek a videa (kodeky, kontejnery)

== Úvodní problematika

- Musíme efektivně reprezentovat to, co vidíme (věrně, či kreativně)
- Ale foťáky nejsou perfektní, formáty nejsou perfektní, a displeje nejsou perfektní
  - (A paměť není nekonečná, a internet není instantní)
- Tak děláme kompromisy a píčoviny pro nejlepší média pro danou situaci

= Barevný prostor (Color space)

==
- Matematický popis barev
- Barevných prostorů existuje řada, rozlišujeme pohled fotografický a pro digitální média
- Složky:
  - *Gamut*: Rozpětí barev, které daný prostor dokáže popsat
    - Trojúhelník na CIE 1931 chromaticity diagramu
    - Větší trojúhelník = širší gamut = více saturovaných barev
  - *White Point*: Referenční bod pro bílou barvu
    - Standard D65 (6500K) - denní světlo
    - D50 (5000K) pro tisk
  - *Gamma*: Nelineární převodní funkce mezi signálem a jasem
    - Lidské oko je citlivější na změny v tmavých tónech
    - Gamma encoding alokuje více dat pro tmavé oblasti

==
#align(center)[
#image("CIE-1931_diagram_in_LAB_space.svg.png", height: 80%)
]

= Nejpoužívanější color spaces

==
- *sRGB / Rec. 709*: Základ pro web a HD video
  - Nejmenší gamut, ale nejširší podpora
  - Gamma 2.2 (sRGB) nebo 2.4 (Rec. 709)
- *Adobe RGB*: Pro fotografii a tisk
  - Širší gamut než sRGB, zejména v zelených a azurových
- *DCI-P3*: Digitální kino a moderní displeje
  - 26% větší než sRGB
  - Apple displeje, HDR content

==
- *Rec. 2020*: UHD/4K/8K a HDR video
  - Obrovský gamut pro budoucnost
  - Displeje pokryjí jen část

= Modely barev

==
- *RGB*: Aditivní model pro zachycení a zobrazení světla
- *YCbCr*: Oddělení jasu (Y') a barevných složek (Cb, Cr)
  - Efektivnější pro kompresi videa
  - Lidské oko je citlivější na jas než barvu
    - Toho zneužívá např. JPEG (o tom později)
- *CMYK*: Subtraktivní model pro tisk (typicky ne pro digitální média)
- *HSL/HSV*: Intuitivní pro úpravy (odstín, saturace, světlost)

= RGB

==
- Červená, zelená, modrá - základní barvy světla
- Každý kanál typicky 0-255 (8-bit) nebo 0-1023 (10-bit)
- Přímo odpovídá fyzickým subpixelům displeje
- Všechny barvy vznikají kombinací těchto tří složek
- Neefektivní pro kompresi - všechny kanály stejně důležité

= YCbCr

==
- Y' (Luma): Jasová složka s detaily obrazu
- Cb: Modrá minus jas (B-Y')
- Cr: Červená minus jas (R-Y')
- Výhody:
  - Lze redukovat barevné kanály bez ztráty detailů
  - Základ pro chroma subsampling (4:2:0, 4:2:2)
- Používá se v JPEG, H.264, H.265
- Nevýhody:
  - Není tak lidsky intuitivní

==
#align(center)[
#image("CCD.png", height: 80%)
]

= HSL a HSV
==
- Hue, Saturation, Lightness, Value
- Velmi intuitivní pro lidské chápání a popis barev
- Odstín (H): Samotná barva na barevném kruhu (0-360°)
- Sytost (S): Čistota/intenzita barvy (0 % je šedá, 100 % je plná barva)
- Světlost (L): Jak je barva světlá nebo tmavá (0 % je černá, 50 % je čistá barva, 100 % je bílá)
  - Value je podobná světlosti, ale vede od černé (0%) do nejjasnější verze barvy (ne k bílé)
- Intuitivní, ale neuniformní na vnímání (oko nevnímá 50% světlost u všech barev stejně jasně)

#align(center)[
#image("Hsl-hsv_models.svg.png")
]

= Barevná bitová hloubka

==
- Počet možných hodnot pro každý barevný kanál
- *8-bit*: 256 hodnot/kanál, 16.7 milionů barev celkem
  - Standard pro JPEG, web
  - Může způsobit banding v gradientech
- *10-bit*: 1024 hodnot/kanál, 1.07 miliardy barev
  - HDR video, profi displeje
- *12/14/16-bit*: RAW fotografie
  - Mraky barev, nechce se mi počítat
  - Více prostoru pro úpravy bez degradace
- Většina displejů stejně umí max 10 bit...

==
#image("Gradient-Bit-Depth-Test-File-Greg-Benz-1.jpg", height: 80%)

= Zachytávání obrazu
==
- Senzor měří pouze intenzitu světla, ne barvu
- *Bayerův filtr*: Mřížka RGGB filtrů před senzorem
  - 50% zelená (lidské oko je nejcitlivější na zelenou, protože v přírodě je všude)
  - 25% červená, 25% modrá
- *X-Trans* (Fujifilm): 6x6 vzor, méně moiré efektu
- *Demosaicing*: Proces interpolace plnobarevného obrazu z mozaiky senzoru

==
#align(center)[
#image("{8CBEA350-0587-4702-99BA-3C260498AD6A}.png", height: 80%)
]
==
#align(center)[
#image("Bayer_pattern_on_sensor.svg.png", height: 80%)
]
==
#align(center)[
#image("1__rxiwcCQ7K46IVOzzG8Yaw.png", height: 80%)
]
==
#align(center)[
#image("{7BE189EC-B229-4F06-8932-7B82847F1962}.png", height: 80%)
]
= Formáty pro zachytávání

==
- *RAW*: Surová data ze senzoru
  - Vysoká bitová hloubka (12-16 bit)
  - White balance atp. jako metadata
  - Maximální flexibilita v post-produkci
  - Ale každý výrobce má vlastní RAW formát
    -> Adobe DNG Converter (DNG: Digital Negative)

==
- *Log profily* (video): S-Log, V-Log, F-Log
  - Logaritmická gamma křivka
  - Zachová detaily ve světlech i stínech
- *JPEG/H.264*: Zpracované v kameře
  - Rychlé, malé soubory
  - Omezená flexibilita

= Formáty pro export a zpracování

==
Rozlišujeme:
- *Kontejner*: Obsahuje data videa/obrázku (MP4, MOV, MKV, webp)
- *Kodek*: Kompresní algoritmus (H.264, HEVC, ProRes)
   - codec = #strong[co]der, #strong[dec]oder
- *Ztrátová komprese*: Zahazuje data pro menší soubory
  - Formáty jsou typicky optimalizované, aby zahazovaly detaily tam, kde to lidskému oku nevadí
- *Bezeztrátová*: Zachovává všechna data
  - Praktikujeme v podstatě jen u obrázků/fotek
  
= JPEG
==
- Nejrozšířenější formát pro fotografie
- Ztrátová komprese založená na DCT
- YCbCr s chroma subsamplingem
- Jen 8-bit barevná hloubka :(
- Doporučuje se kvalita 80-90 optimální poměr kvalita/velikost, ale who cares, JPEGy jsou malý tak jako tak
- Nevhodný pro digitální grafiku

==
#align(center)[
#image("Examples-of-JPEG-artifacts-at-a-high-compression-ratio-At-a-high-compression-ratio-in.png", height: 80%)
]
==
#align(center)[
#image("An-example-of-artefacts-created-during-lossy-JPEG-compression-with-quality-q-60-Each.png", height: 80%)
]
= HEIF

==
- High Efficiency Image Format (Apple od iOS 11)
- Založeno na HEVC video kodeku (kterej zatěžují mraky patentů)
- 50% velikost oproti JPEG při stejné kvalitě
- Podporuje 16-bit, HDR, sérii obrázků, nedestruktivní úpravy, informace o hloubce (pro depth of field/bokeh)
- Omezená podpora mimo Apple ekosystém
  - A hlavně to neuměj prohlížeče, což je tragédie
  - Photoshop umí jen 8-bit barevnou hloubku

= TIFF

==
- Tagged Image File Format
- Bezeztrátová komprese (LZW, ZIP) nebo nekomprimovaný
- Podporuje 16-bit, CMYK, vrstvy
- Standard pro archivaci a profesionální tisk
- Ale relativně velké soubory

= PNG

==
- Portable Network Graphics
- Vytvořen, aby nahradil GIF (Graphics Interchange Format), nejděsivější formát obrázků na planetě
- Bezeztrátová komprese
- Alfa kanál pro průhlednost (JPEG neumí průhlednost vůbec, GIF jen binárně)
- Grafika, screenshoty, loga... fotky moc ne
- Větší než JPEG pro fotografie

= WebP

==
- Google formát pro web
- Ztrátová i bezeztrátová komprese
  - Past na smrtelníky
- 25-35% menší než JPEG
- Podporuje animace a průhlednost
- Široká podpora v prohlížečích
- Na fotky se moc nepoužívá

= MP4

==
- MPEG-4 Part 14 kontejner
- Univerzální podpora napříč zařízeními
  - V podstatě všechno umí hrát MP4
- Může obsahovat H.264, H.265, AV1
- Podporuje multiple audio/video tracks
- Metadata a kapitoly

= MOV

==
- Apple QuickTime kontejner
- Profesionální video workflow
- Podporuje ProRes, DNxHD/HR
- Reference movie capability
- Větší overhead než MP4 soubory
  - To ale většinou nevadí

= MKV
==
- Matroska, open-source kontejner
- Neomezené množství stop
  - Hypoteticky jeden MKV může obsahovat N videí a M zvukových stop a klidně obrázky a titulky k tomu
- Podporuje všechny kodeky
- Ideální pro archivaci
- Kapitoly, menu, metadata

= WebM
==
- Open-source pro web (Google)
  - Formálně je WebM blbějši verze Matrosky
- Pouze VP8/VP9/AV1 video
  - Protože Google a další nechtějí řešit patenty
- Pouze Vorbis/Opus audio
- Optimalizováno pro streaming

= Kodeky

==
Kompresní algoritmy pro video:
- *Intra-frame*: Každý snímek komprimován samostatně (ProRes, DNxHD)
  - To moc smrtelníky nezajímá, protože
- *Inter-frame*: Komprese mezi snímky (H.264, H.265)
  - I-frames: Kompletní snímky
  - P-frames: Predikce z předchozích
  - B-frames: Obousměrné predikce

==
#align(center)[
#image("frames.png")
]

= H.264
==
- AVC (Advanced Video Coding)
- Nejrozšířenější kodek současnosti
- Dobrý poměr kvalita/velikost
- Hardware akcelerace všude
- 4:2:0 chroma subsampling standard
  - Chroma subsampling si vysvětlíme za chvíli
- Max 4K rozlišení

= H.265/HEVC
==
- High Efficiency Video Coding
- 50% úspora oproti H.264
- Nativní podpora pro 10-bit barvy a HDR
- Max 8K rozlišení
- Vyšší nároky na výpočet
- Licenční problémy

= AV1
==
- Alliance for Open Media
- Royalty-free alternativa k HEVC
  - Myšlenka je, ať si patent-holdeři HEVC jdou nasrat
- Až o 30% efektivnější než HEVC
- YouTube, Netflix adoptují
- Extrémně náročný encoding (to ale nevadí)
- Decoding záleží -- na řadě platforem chybí hardware akcelerace

= Chroma subsampling
==
J:a:b notace:
- *4:4:4*: Plné rozlišení barvy - profesionální
- *4:2:2*: Poloviční horizontální - broadcast
- *4:2:0*: Čtvrtinové rozlišení barvy - streaming, Blu-ray

Příklad pro 4x2 pixely:
- 4:4:4 = 24 vzorků (8Y + 8Cb + 8Cr)
- 4:2:0 = 12 vzorků (8Y + 2Cb + 2Cr)

==
#align(center)[
#image("Common_chroma_subsampling_ratios_YCbCr_CORRECTED.svg.png", height: 80%)
]

= Krok 1: Technická korekce
==
V první řadě chceme co nejlepší nastavení ve foťáku, obzvlášť, pokud nepracujeme s RAW soubory.

Čištění obrazu před kreativními úpravami:
- Odstranění technických nedostatků
- Příprava čistého základu
- Zachování maximálních detailů

= Denoising
==
- Digitální šum z vysokého ISO nebo nedostatečného osvětlení
  - To v praxi znamená to samý
- Typy šumu:
  - Luminance noise: zrnitost v jasu
  - Chroma noise: barevné fleky
- Temporal denoising pro video (průměrování snímků)
- Balance mezi čistotou a zachováním detailů

==
#align(center)[
#image("An-example-of-denoising-an-image.jpg")
]
= Chromatická aberace
==
- Barevné lemování kolem kontrastních hran
- Způsobeno nedokonalostí čoček v objektivu
- Typicky ialové nebo zelené okraje
- Foťáky a nástroje často dělají automatickou korekci

==
#align(center)[
#image("Header_AddingChromaticAbberation_JFamiglieti.png", height: 80%)
]

= Krok 2: Color correction
==
Globální úpravy pro neutrální, vyvážený obraz:
- Cíl: technicky správný obraz
- Korekce expozice a kontrastu
- Neutralizace barevných nádechů
- Příprava pro kreativní grading

= Exposure a kontrast
==
- *Lift*: Stíny (blacks/shadows)
- *Gamma*: Střední tóny (midtones)  
- *Gain*: Světla (highlights/whites)
- Hodí se histogramy v editorech, aby nás nešálil zrak
- Zachování detailů v extrémech

==
#align(center)[
#image("primaries-color-wheels.jpg", height: 80%)
]

= White balance
==
- Korekce barevné teploty (v Kelvinech reprezentující teplotu černého tělesa)
  - 2800K: žárovka (teplá/oranžová)
  - 5600K: denní světlo
  - 7000K+: zataženo (studená/modrá)
- Tint korekce: Zeleno-purpurová osa
- Referenční bod: neutrální šedá/bílá
- Foťák to má obráceně, protože má kompenzovat prostředí
  - Jako jsme to viděli na našem Fujifilmu

= Saturace
==
- Globální intenzita barev
- Jde ladit jednotlivé kanály
- Přesycení neboli clipping == velký špatný

= Krok 3: Color grading
==
Kreativní fáze - hehe:
- Vše dříve zmíněné, plus LUTs a specifické efekty

= LUTs
==
Look-Up Tables - převodní tabulky funkcí konvertujících pixel na pixel:
- *1D LUT*: Pouze úprava jasu/kontrastu
- *3D LUT*: Komplexní barevné transformace
- Technical LUTs: Konverze prostorů
- Creative LUTs: Stylistické efekty

==
#align(center)[
#image("3a.jpg", height: 80%)
]

= Efekty
==
Dokončovací prvky:
- *Film grain*
- *Vignette*: Ztmavení rohů, fokus na střed
- *Glow/Halation*: Měkké rozptýlení světel
- *Split toning*: Různé barvy pro světla/stíny

==
#align(center)[
#image("{A619B0DC-3D73-448B-8892-2DABFD764C7A}.png", height: 80%)
]

==
#align(center)[
#image("{BFA2E3A6-8556-4BDC-BDB5-1C41B934D96F}.png", height: 80%)
]
==
#align(center)[
#image("{DEF5F7B8-B368-4098-A85B-CDA73B47DD50}.png", height: 80%)
]

#image("27g4bvav6zae1.png")
== Nástroje
- DaVinci Resolve
- Adobe Premiere Pro a Lightroom
- Adobe DNG Converter
  - Protože žádnej color-grading program neumí všechny RAW formáty :)

= Otázky?