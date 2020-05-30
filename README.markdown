[![Build Status](https://secure.travis-ci.org/rolandwalker/unicode-fonts.png?branch=master)](http://travis-ci.org/rolandwalker/unicode-fonts)

# unicode-fonts

Configure Unicode fonts for Emacs.

![Using the Native Mac backend](https://raw.githubusercontent.com/rolandwalker/unicode-fonts/master/native_mac_backend.png)

 * [Quickstart](#quickstart)
 * [Testing](#testing)
 * [Customization](#customization)
 * [Overview](#overview)
 * [Minimum Useful Fonts](#minimum-useful-fonts)
 * [Startup Speed](#startup-speed)
 * [Unmapped Blocks](#unmapped-blocks)
 * [Emoji](#emoji)
 * [Bugs](#bugs)
 * [Free International and Symbol Fonts](#free-international-and-symbol-fonts)
 * [Non-free Fonts](#non-free-fonts)
 * [Chinese and Arabic Scripts](#chinese-and-arabic-scripts)
 * [Unicode "Scripts"](#unicode-scripts)
 * [Compatibility and Requirements](#compatibility-and-requirements)

## Quickstart

 * Remove Unifont from your system.

 * Install these fonts
	* <https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip>
	* <http://www.quivira-font.com/files/Quivira.ttf>    ; or Quivira.otf
	* <http://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2>
	* <https://github.com/googlei18n/noto-fonts/raw/master/hinted/NotoSans-Regular.ttf>
	* <https://github.com/googlei18n/noto-fonts/raw/master/unhinted/NotoSansSymbols-Regular.ttf>

 * Use an extended Latin font for your default face, such
   as Monaco, Consolas, or DejaVu Sans Mono.

```elisp
(require 'unicode-fonts)
(unicode-fonts-setup)
```

## Testing

Keystrokes                                                                                | Notes
------------------------------------------------------------------------------------------|--------------------------------
<kbd>C-h</kbd> <kbd>h</kbd>                                                               | same as <kbd>M-x</kbd> <kbd>view-hello-file</kbd>
<kbd>M-x</kbd> <kbd>list-charset-chars</kbd> <kbd>RET</kbd> <kbd>unicode-bmp</kbd> <kbd>RET</kbd>                          | search for *eg* 210x
<kbd>M-x</kbd> <kbd>list-charset-chars</kbd> <kbd>RET</kbd> <kbd>unicode-smp</kbd> <kbd>RET</kbd>                          | if your display backend supports astral chars
<kbd>M-x</kbd> <kbd>unicode-fonts-debug-insert-block</kbd> <kbd>RET</kbd> <kbd>Mathematical_Operators</kbd> <kbd>RET</kbd> |

## Customization

<kbd>M-x</kbd> <kbd>customize-group</kbd> <kbd>RET</kbd> <kbd>unicode-fonts</kbd> <kbd>RET</kbd>

## Overview

Emacs maintains font mappings on a per-glyph basis, meaning
that multiple fonts are used at the same time (transparently) to
display any character for which you have a font.  Furthermore,
Emacs does this out of the box.

However, font mappings via fontsets are a bit difficult to
configure.  In addition, the default setup does not always pick
the most legible fonts.  As the manual warns, the choice of font
actually displayed for a non-ASCII character is "somewhat random".

The Unicode standard provides a way to organize font mappings: it
divides character ranges into logical groups called "blocks".  This
library configures Emacs in a Unicode-friendly way by providing
mappings from

	each Unicode block  ---to--->   a font with good coverage

and makes the settings available via the customization interface.

This library provides font mappings for 233 of the 255 blocks in
the Unicode 8.0 standard which are public and have displayable
characters.  It assumes that 6 Latin blocks are covered by the
default font.  16/255 blocks are not mapped to any known font.

To use unicode-fonts, place the `unicode-fonts.el` file somewhere
Emacs can find it, and add the following to your `~/.emacs` file:

```elisp
(require 'unicode-fonts)
(unicode-fonts-setup)
```

See important notes about startup speed below.

## Minimum Useful Fonts

To gain any benefit from the library, you must have fonts with good
Unicode support installed on your system.  If you are running a
recent version of OS X or Microsoft Windows, you already own some
good multi-lingual fonts, though you would do very well to download
and install the six items below:

From <https://dejavu-fonts.github.io/Download.html>

	DejaVu Sans, DejaVu Sans Mono

From <http://www.quivira-font.com/downloads.php>

	Quivira

From <https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip>

	Symbola

From <http://www.google.com/get/noto/>

	Noto Sans, Noto Sans Symbols

It is also recommended to remove GNU Unifont from your system.
Unifont is very useful for debugging, but not useful for reading.

## Startup Speed

The default options favor correctness and completeness over speed, and can
add many seconds to initial startup time in GUI mode.

However, when possible a font cache is kept between sessions.  If you
have [persistent-soft.el](http://github.com/rolandwalker/persistent-soft) installed, **when you start Emacs the second time, the
startup cost should be negligible**.

The disk cache will be rebuilt during Emacs startup whenever a font is added
or removed, or any relevant configuration variables are changed. To increase
the speed of occasionally building the disk cache, you may use the
customization interface to remove fonts from `unicode-fonts-block-font-mapping`
which are not present on your system.

## Unmapped Blocks

On the assumption that an extended Latin font such as Monaco,
Consolas, or DejaVu Sans Mono is already being used for the default
face, no separate mappings are provided for the following Unicode
blocks:

	Basic Latin
	Latin Extended Additional
	Latin Extended-A
	Latin Extended-B
	Latin-1 Supplement
	Spacing Modifier Letters

though some of these remain configurable via `customize`.

## Emoji

Color Emoji are enabled by default when using the Native Mac port
on OS X.  This can be disabled by customizing each relevant mapping,
or by turning off all multicolor glyphs here:

<kbd>M-x</kbd> <kbd>customize-variable</kbd> <kbd>RET</kbd> <kbd>unicode-fonts-skip-font-groups</kbd> <kbd>RET</kbd>

## Bugs

Calling `set-fontset-font` can easily crash Emacs.  There is a
workaround, but it may not be sufficient on all platforms.
Tested on Cocoa Emacs, Native Mac Emacs, X11/XQuartz,
MS Windows XP.

Widths of alternate fonts do not behave as expected on MS Windows.
For example, DejaVu Sans Mono box-drawing characters may use a
different width than the default font.

## Free International and Symbol Fonts

Free fonts recognized by this package may be downloaded from the
following locations.  For any language, it is increasingly likely
that Noto Sans provides coverage:

From <http://www.google.com/get/noto/>

	Noto Sans and friends         ; 181 Unicode blocks and counting; sole
	                              ; source for these blocks:
	                              ;
	                              ;   Bamum / Bamum Supplement / Kaithi
	                              ;   Mandaic / Meetei Mayek Extensions
	                              ;   Sundanese Supplement
	                              ;
	                              ; Also a good source for recently-added
	                              ; glyphs such as "Turkish Lira Sign".

From <http://scripts.sil.org/cms/scripts/page.php?item_id=CharisSIL_download>  
  or <http://scripts.sil.org/cms/scripts/page.php?item_id=DoulosSIL_download>

	Charis SIL or Doulos SIL      ; Extended European and diacritics

From <http://scripts.sil.org/cms/scripts/page.php?item_id=Gentium_download>

	Gentium Plus                  ; Greek

From <http://users.teilar.gr/~g1951d/>    ; *NOTE: site is down as of July 2015*

	Aegean, Aegyptus, Akkadian    ; Ancient languages
	Analecta                      ; Ancient languages, Deseret
	Anatolian                     ; Ancient languages
	Musica                        ; Musical Symbols
	Nilus                         ; Ancient languages

From <http://www.wazu.jp/gallery/views/View_MPH2BDamase.html>

	MPH 2B Damase                 ; Arabic, Armenian, Buginese, Cherokee, Georgian,
	                              ; Glagolitic, Hanunoo, Kharoshthi, Limbu, Osmanya,
	                              ; Shavian, Syloti Nagri, Tai Le, Thaana

From <http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=NamdhinggoSIL>

	Namdhinggo SIL                ; Limbu

From <http://wenq.org/wqy2/index.cgi?FontGuide>

	WenQuanYi Zen Hei             ; CJK (Simplified Chinese)

From <http://babelstone.co.uk/Fonts/>

	BabelStone Han                ; CJK (Simplified Chinese)
	BabelStone Phags-pa Book      ; Phags-pa
	BabelStone Modern             ; Tags / Specials / Selectors

From <http://vietunicode.sourceforge.net/fonts/fonts_hannom.html>

	HAN NOM A, HAN NOM B          ; CJK (Nôm Chinese)

From <http://kldp.net/projects/unfonts/>

	Un Batang                     ; CJK (Hangul)

From <http://sourceforge.jp/projects/hanazono-font/releases/>

	Hana Min A, Hana Min B        ; CJK (Japanese)

From <http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=SILYi_home>

	Nuosu SIL                     ; CJK (Yi)

From <http://www.daicing.com/manchu/index.php?page=fonts-downloads>

	Daicing Xiaokai               ; Mongolian

From <http://www.library.gov.bt/IT/fonts.html>

	Jomolhari                     ; Tibetan

From <http://www.thlib.org/tools/scripts/wiki/tibetan%20machine%20uni.html>

	Tibetan Machine Uni           ; Tibetan

From <http://scripts.sil.org/cms/scripts/page.php?item_id=Padauk>

	Padauk                        ; Myanmar

From <https://code.google.com/p/myanmar3source/downloads/list>

	Myanmar3                      ; Myanmar

From <http://www.yunghkio.com/unicode/>

	Yunghkio                      ; Myanmar

From <https://code.google.com/p/tharlon-font/downloads/list>

	TharLon                       ; Myanmar

From <http://sourceforge.net/projects/prahita/files/Myanmar%20Unicode%20Fonts/MasterpieceUniSans/>

	Masterpiece Uni Sans          ; Myanmar

From <http://sarovar.org/projects/samyak/>

	Samyak                        ; Gujarati, Malayalam, Oriya, Tamil

From <http://software.sil.org/annapurna/download/>

	Annapurna SIL                 ; Devanagari

From <http://guca.sourceforge.net/typography/fonts/anmoluni/>

	AnmolUni                      ; Gurmukhi

From <http://brahmi.sourceforge.net/downloads2.html>

	Kedage                        ; Kannada

From <http://www.omicronlab.com/bangla-fonts.html>

	Mukti Narrow                  ; Bengali

From <http://www.kamban.com.au/downloads.html>

	Akshar Unicode                ; Sinhala

From <http://tabish.freeshell.org/eeyek/download.html>

	Eeyek Unicode                 ; Meetei Mayek

From <http://scripts.sil.org/CMS/scripts/page.php?&item_id=Mondulkiri>

	Khmer Mondulkiri              ; Khmer

From <http://www.laoscript.net/downloads/>

	Saysettha MX                  ; Lao

From <http://www.geocities.jp/simsheart_alif/taithamunicode.html>

	Lanna Alif                    ; Tai Tham

From <http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=DaiBannaSIL>

	Dai Banna SIL                 ; New Tai Lue

From <http://scripts.sil.org/cms/scripts/page.php?item_id=TaiHeritage>

	Tai Heritage Pro              ; Tai Viet

From <http://sabilulungan.org/aksara/>

	Sundanese Unicode             ; Sundanese

From <http://www.amirifont.org/>

	Amiri                         ; Arabic (Naskh)

From <http://scripts.sil.org/cms/scripts/page.php?item_id=Scheherazade>

	Scheherazade                  ; Arabic (Naskh)

From <http://www.farsiweb.ir/wiki/Persian_fonts>

	Koodak                        ; Arabic (Farsi)

From <http://openfontlibrary.org/font/ahuramazda/>

	Ahuramzda                     ; Avestan

From <http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=AbyssinicaSIL>

	Abyssinica SIL                ; Ethiopic

From <http://www.bethmardutho.org/index.php/resources/fonts.html>

	Estrangelo Nisibin            ; Syriac

From <http://www.evertype.com/fonts/nko/>

	Conakry                       ; N'ko

From <http://uni.hilledu.com/download-ribenguni>

	Ribeng                        ; Chakma

From <http://www.virtualvinodh.com/downloads>

	Adinatha Tamil Brahmi         ; Brahmi

From <http://ftp.gnu.org/gnu/freefont/>

	FreeMono, etc (FreeFont)      ; Kayah Li (and others)

From <http://ulikozok.com/aksara-batak/batak-font/>

	Batak-Unicode                 ; Batak

From <http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=Mingzat>

	Mingzat                       ; Lepcha

From <http://phjamr.github.io/lisu.html#install>  
 <http://phjamr.github.io/miao.html#install>  
 <http://phjamr.github.io/mro.html#install>  

	Lisu Unicode                  ; Lisu
	Miao Unicode                  ; Miao
	Mro Unicode                   ; Mro

From <http://scholarsfonts.net/cardofnt.html>

	Cardo                         ; Historical Languages

From <http://sourceforge.net/projects/junicode/files/junicode/>

	Junicode                      ; Historical Languages

From <http://www.evertype.com/fonts/vai/>

	Dukor                         ; Vai

From <http://sourceforge.net/projects/zhmono/>

	ZH Mono                       ; Inscriptional Pahlavi / Parthian

From <http://culmus.sourceforge.net/ancient/index.html>

	Aramaic Imperial Yeb          ; Imperial Aramaic

From <http://www.languagegeek.com/font/fontdownload.html>

	Aboriginal Sans               ; Aboriginal Languages
	Aboriginal Serif

From <http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=EzraSIL_Home>

	Ezra SIL                      ; Hebrew

From <http://www.evertype.com/fonts/coptic/>

	Antinoou                      ; Coptic / General Punctuation

From <http://apagreekkeys.org/NAUdownload.html>

	New Athena Unicode            ; Ancient Languages / Symbols

From <http://markmail.org/thread/g57mk4sbdycblxds>

	KhojkiUnicodeOT               ; Khojki

From <https://github.com/andjc/ahom-unicode/tree/master/font>

	AhomUnicode                   ; Ahom

From <https://github.com/MihailJP/oldsindhi/releases>

	OldSindhi                     ; Khudawadi

From <https://github.com/MihailJP/Muktamsiddham/releases>

	MuktamsiddhamG                ; Siddham  (note trailing "G" on font name)

From <https://github.com/MihailJP/MarathiCursive/releases>

	MarathiCursiveG               ; Modi  (note trailing "G" on font name)

From <https://github.com/OldHungarian/old-hungarian-font/releases>

	OldHungarian                  ; Old Hungarian

From <http://tutohtml.perso.sfr.fr/unicode.html>

	Albanian                      ; Elbasan / Takri / Sharada

From <https://github.com/enabling-languages/cham-unicode/tree/master/fonts/ttf>

	Cham OI_Tangin                ; Cham

From <https://ctan.org/tex-archive/fonts/Asana-Math?lang=en>

	Asana Math                    ; Mathematical Symbols

## Non-free Fonts

Many non-free fonts are referenced by the default settings.
However, free alternatives are also given wherever possible, and
patches are of course accepted to improve every case.

## Chinese and Arabic Scripts

If you are using a language written in Chinese or Arabic script,
try customizing `unicode-fonts-skip-font-groups` to control which
script you see, and send a friendly bug report.

## Unicode "Scripts"

Unicode also defines the notion of a "script" as a higher-level
abstraction which is independent of "blocks".  Modern fonts can
report their script coverage, and Emacs may also access that
information.  However, this library ignores scripts in favor
of blocks and glyphs.

## Compatibility and Requirements

	GNU Emacs version 23.3 and higher : yes
	GNU Emacs version 22.3 and lower  : no

Requires [fonts-utils.el](http://github.com/rolandwalker/font-utils), [ucs-utils.el](http://github.com/rolandwalker/ucs-utils)

Uses if present: [persistent-soft.el](http://github.com/rolandwalker/persistent-soft) (Recommended)

<!-- Emacs
Local Variables:
coding: utf-8
End:

LocalWords:  quickstart emoji chinese Unifont Quivira Consolas DejaVu elisp
LocalWords:  arabic charset Symbola Noto fontset XQuartz Bamum Kaithi multi
LocalWords:  Mandaic Meetei Mayek Doulos Gentium Akkadian Analecta Khojki
LocalWords:  Musica Nilus Deseret Buginese Hanunoo Kharoshthi Limbu Ahom
LocalWords:  Osmanya Syloti Nagri Thaana Phags Namdhinggo WenQuanYi Siddham
LocalWords:  BabelStone Damase Batang Nuosu Xiaokai Daicing Jomolhari Modi
LocalWords:  Padauk Yunghkio TharLon Samyak AnmolUni Kedage Mukti OldSindhi
LocalWords:  Akshar Gurmukhi Eeyek Mondulkiri Saysettha Alif Tham Khudawadi
LocalWords:  Banna Amiri Avestan Ahuramzda Koodak Naskh Ethiopic N'ko Takri
LocalWords:  Abyssinica Estrangelo Nisibin Chakma Ribeng Adinatha Elbasan
LocalWords:  Brahmi FreeMono FreeFont Kayah Batak Mingzat Lepcha Lisu devel
LocalWords:  Miao Cardo Junicode Dukor Parthian Antinoou KhojkiUnicodeOT
LocalWords:  AhomUnicode MuktamsiddhamG MarathiCursiveG OldHungarian
LocalWords:  Sharada
 -->
