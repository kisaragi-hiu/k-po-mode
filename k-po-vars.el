;;; k-po-vars.el --- Variables to be seen in all files -*- lexical-binding: t -*-

;;; Commentary:

;; In a multi-file package setup like this, it's harder to declare variables
;; before they are used, especially when individual files need to be able to be
;; byte-compiled individually.
;;
;; This works around that.

;;; Code:

;;;; User options

(defgroup k-po nil
  "Major mode for editing PO files."
  :group 'i18n)

(defcustom k-po-auto-edit-with-msgid nil
  "*Automatically use msgid when editing untranslated entries."
  :type 'boolean
  :group 'k-po)

(defcustom k-po-auto-fuzzy-on-edit nil
  "*Automatically mark entries fuzzy when being edited."
  :type 'boolean
  :group 'k-po)

(defcustom k-po-auto-delete-previous-msgid t
  "*Automatically delete previous msgid (marked #|) when editing entry.
Value is nil, t, or ask."
  :type '(choice (const nil)
          (const t)
          (const ask))
  :group 'k-po)

(defcustom k-po-auto-select-on-unfuzzy nil
  "*Automatically select some new entry while making an entry not fuzzy."
  :type 'boolean
  :group 'k-po)

(defcustom k-po-keep-mo-file nil
  "*Set whether MO file should be kept or discarded after validation."
  :type 'boolean
  :group 'k-po)

(defcustom k-po-auto-update-file-header 'ask
  "*Automatically revise headers.  Value is nil, t, or ask."
  :type '(choice (const nil)
          (const t)
          (const ask))
  :group 'k-po)

(defcustom k-po-auto-replace-revision-date t
  "*Automatically revise date in headers.  Value is nil, t, or ask."
  :type '(choice (const nil)
          (const t)
          (const ask))
  :group 'k-po)

(defcustom k-po-default-file-header "\
# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid \"\"
msgstr \"\"
\"Project-Id-Version: PACKAGE VERSION\\n\"
\"PO-Revision-Date: YEAR-MO-DA HO:MI +ZONE\\n\"
\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"
\"Language-Team: LANGUAGE <LL@li.org>\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=CHARSET\\n\"
\"Content-Transfer-Encoding: 8bit\\n\"
"
  "*Default PO file header."
  :type 'string
  :group 'k-po)

(defcustom k-po-translation-project-address
  "robot@translationproject.org"
  "*Electronic mail address of the Translation Project.
Typing \\[k-po-send-mail] (normally bound to `M') the user will send the PO file
to this email address."
  :type 'string
  :group 'k-po)

(defcustom k-po-translation-project-mail-label "TP-Robot"
  "*Subject label when sending the PO file to `k-po-translation-project-address'."
  :type 'string
  :group 'k-po)

(defcustom k-po-highlight-face 'highlight
  "*The face used for PO mode highlighting.  For Emacses with overlays.
Possible values are `highlight', `modeline', `secondary-selection',
`region', and `underline'.
This variable can be set by the user to whatever face they desire.
It's most convenient if the cursor color and highlight color are
slightly different."
  :type 'face
  :group 'k-po)

(defcustom k-po-team-name-to-code
  ;; All possible languages, a complete ISO 639 list, the inverse of
  ;; gettext-tools/src/lang-table.c, and a little more.
  '(("LANGUAGE" . "LL")
    ("(Afan) Oromo" . "om")
    ("Abkhazian" . "ab")
    ("Achinese" . "ace")
    ("Afar" . "aa")
    ("Afrikaans" . "af")
    ("Akan" . "ak")
    ("Albanian" . "sq")
    ("Amharic" . "am")
    ("Arabic" . "ar")
    ("Aragonese" . "an")
    ("Argentinian" . "es_AR")
    ("Armenian" . "hy")
    ("Assamese" . "as")
    ("Austrian" . "de_AT")
    ("Avaric" . "av")
    ("Avestan" . "ae")
    ("Awadhi" . "awa")
    ("Aymara" . "ay")
    ("Azerbaijani" . "az")
    ("Balinese" . "ban")
    ("Baluchi" . "bal")
    ("Bambara" . "bm")
    ("Bashkir" . "ba")
    ("Basque" . "eu")
    ("Beja" . "bej")
    ("Belarusian" . "be")
    ("Bemba" . "bem")
    ("Bengali" . "bn")
    ("Bhojpuri" . "bho")
    ("Bihari" . "bh")
    ("Bikol" . "bik")
    ("Bini" . "bin")
    ("Bislama" . "bi")
    ("Bosnian" . "bs")
    ("Brazilian Portuguese" . "pt_BR")
    ("Breton" . "br")
    ("Buginese" . "bug")
    ("Bulgarian" . "bg")
    ("Burmese" . "my")
    ("Catalan" . "ca")
    ("Cebuano" . "ceb")
    ("Central Khmer" . "km")
    ("Chamorro" . "ch")
    ("Chechen" . "ce")
    ("Chinese" . "zh")
    ("Chinese (Hong Kong)" . "zh_HK")
    ("Chinese (simplified)" . "zh_CN")
    ("Chinese (traditional)" . "zh_TW")
    ("Church Slavic" . "cu")
    ("Chuvash" . "cv")
    ("Cornish" . "kw")
    ("Corsican" . "co")
    ("Cree" . "cr")
    ("Croatian" . "hr")
    ("Czech" . "cs")
    ("Danish" . "da")
    ("Dinka" . "din")
    ("Divehi" . "dv")
    ("Dogri" . "doi")
    ("Dutch" . "nl")
    ("Dzongkha" . "dz")
    ("English" . "en")
    ("English (British)" . "en_GB")
    ("Esperanto" . "eo")
    ("Estonian" . "et")
    ("Ewe" . "ee")
    ("Faroese" . "fo")
    ("Fijian" . "fj")
    ("Filipino" . "fil")
    ("Finnish" . "fi")
    ("Fon" . "fon")
    ("French" . "fr")
    ("Frisian" . "fy")
    ("Fulah" . "ff")
    ("Galician" . "gl")
    ("Ganda" . "lg")
    ("Georgian" . "ka")
    ("German" . "de")
    ("Gondi" . "gon")
    ("Greek" . "el")
    ("Guarani" . "gn")
    ("Gujarati" . "gu")
    ("Haitian" . "ht")
    ("Hausa" . "ha")
    ("Hebrew" . "he")
    ("Herero" . "hz")
    ("Hiligaynon" . "hil")
    ("Hindi" . "hi")
    ("Hiri Motu" . "ho")
    ("Hmong" . "hmn")
    ("Hungarian" . "hu")
    ("Hyam" . "jab")
    ("Icelandic" . "is")
    ("Ido" . "io")
    ("Igbo" . "ig")
    ("Iloko" . "ilo")
    ("Indonesian" . "id")
    ("Interlingua" . "ia")
    ("Interlingue" . "ie")
    ("Inuktitut" . "iu")
    ("Inupiak" . "ik")
    ("Irish" . "ga")
    ("Italian" . "it")
    ("Japanese" . "ja")
    ("Javanese" . "jv")
    ("Jju" . "kaj")
    ("Kabardian" . "kbd")
    ("Kabyle" . "kab")
    ("Kagoma" . "kdm")
    ("Kalaallisut" . "kl")
    ("Kamba" . "kam")
    ("Kannada" . "kn")
    ("Kanuri" . "kr")
    ("Kashmiri" . "ks")
    ("Kashubian" . "csb")
    ("Kazakh" . "kk")
    ("Khmer" . "km") ; old name
    ("Kikuyu" . "ki")
    ("Kimbundu" . "kmb")
    ("Kinyarwanda" . "rw")
    ("Kirghiz" . "ky")
    ("Kirundi" . "rn")
    ("Komi" . "kv")
    ("Kongo" . "kg")
    ("Konkani" . "kok")
    ("Korean" . "ko")
    ("Kuanyama" . "kj")
    ("Kurdish" . "ku")
    ("Kurukh" . "kru")
    ("Laotian" . "lo")
    ("Latin" . "la")
    ("Latvian" . "lv")
    ("Letzeburgesch" . "lb")
    ("Limburgish" . "li")
    ("Lingala" . "ln")
    ("Lithuanian" . "lt")
    ("Low Saxon" . "nds")
    ("Luba-Katanga" . "lu")
    ("Luba-Lulua" . "lua")
    ("Luo" . "luo")
    ("Macedonian" . "mk")
    ("Madurese" . "mad")
    ("Magahi" . "mag")
    ("Maithili" . "mai")
    ("Makasar" . "mak")
    ("Malagasy" . "mg")
    ("Malay" . "ms")
    ("Malayalam" . "ml")
    ("Maltese" . "mt")
    ("Mandingo" . "man")
    ("Manipuri" . "mni")
    ("Manx" . "gv")
    ("Maori" . "mi")
    ("Marathi" . "mr")
    ("Marshall" . "mh")
    ("Marshallese" . "mh")
    ("Marwari" . "mwr")
    ("Mayan" . "myn")
    ("Mende" . "men")
    ("Minangkabau" . "min")
    ("Moldavian" . "mo")
    ("Mongolian" . "mn")
    ("Mossi" . "mos")
    ("Nahuatl" . "nah")
    ("Nauru" . "na")
    ("Navajo" . "nv")
    ("Ndonga" . "ng")
    ("Neapolitan" . "nap")
    ("Nepali" . "ne")
    ("North Ndebele" . "nd")
    ("Northern Sami" . "se")
    ("Northern Sotho" . "nso")
    ("Norwegian Bokmal" . "nb")
    ("Norwegian Nynorsk" . "nn")
    ("Norwegian" . "no")
    ("Nyamwezi" . "nym")
    ("Nyanja" . "ny")
    ("Nyankole" . "nyn")
    ("Occitan" . "oc")
    ("Ojibwa" . "oj")
    ("Old English" . "ang")
    ("Oriya" . "or")
    ("Ossetian" . "os")
    ("Páez" . "pbb")
    ("Pali" . "pi")
    ("Pampanga" . "pam")
    ("Pangasinan" . "pag")
    ("Pashto" . "ps")
    ("Persian" . "fa")
    ("Polish" . "pl")
    ("Portuguese" . "pt")
    ("Punjabi" . "pa")
    ("Quechua" . "qu")
    ("Rajasthani" . "raj")
    ("Rhaeto-Roman" . "rm") ; old name
    ("Romanian" . "ro")
    ("Romansh" . "rm")
    ("Russian" . "ru")
    ("Samoan" . "sm")
    ("Sango" . "sg")
    ("Sanskrit" . "sa")
    ("Santali" . "sat")
    ("Sardinian" . "sc")
    ("Sasak" . "sas")
    ("Scots" . "gd") ; old name
    ("Scottish Gaelic" . "gd")
    ("Serbian" . "sr")
    ("Serer" . "srr")
    ("Sesotho" . "st")
    ("Setswana" . "tn")
    ("Shan" . "shn")
    ("Shona" . "sn")
    ("Sichuan Yi" . "ii")
    ("Sicilian" . "scn")
    ("Sidamo" . "sid")
    ("Sindhi" . "sd")
    ("Sinhala" . "si")
    ("Sinhalese" . "si")
    ("Siswati" . "ss")
    ("Slovak" . "sk")
    ("Slovenian" . "sl")
    ("Somali" . "so")
    ("Sorbian" . "wen")
    ("South Ndebele" . "nr")
    ("Spanish" . "es")
    ("Spanish (Canary Islands)" . "es_IC")
    ("Sukuma" . "suk")
    ("Sundanese" . "su")
    ("Susu" . "sus")
    ("Swahili" . "sw")
    ("Swedish" . "sv")
    ("Swiss German" . "gsw")
    ("Tagalog" . "tl")
    ("Tahitian" . "ty")
    ("Tajik" . "tg")
    ("Tamil" . "ta")
    ("Tatar" . "tt")
    ("Telugu" . "te")
    ("Tetum" . "tet")
    ("Thai" . "th")
    ("Tibetan" . "bo")
    ("Tigrinya" . "ti")
    ("Timne" . "tem")
    ("Tiv" . "tiv")
    ("Tonga" . "to")
    ("Tsonga" . "ts")
    ("Tumbuka" . "tum")
    ("Turkish" . "tr")
    ("Turkmen" . "tk")
    ("Twi" . "tw")
    ("Tyap" . "kcg")
    ("Uighur" . "ug")
    ("Ukrainian" . "uk")
    ("Umbundu" . "umb")
    ("Urdu" . "ur")
    ("Uzbek" . "uz")
    ("Venda" . "ve")
    ("Vietnamese" . "vi")
    ("Volapuk" . "vo")
    ("Walloon" . "wa")
    ("Walamo" . "wal")
    ("Waray" . "war")
    ("Welsh" . "cy")
    ("Western Frisian" . "fy")
    ("Wolof" . "wo")
    ("Xhosa" . "xh")
    ("Yao" . "yao")
    ("Yiddish" . "yi")
    ("Yoruba" . "yo")
    ("Zapotec" . "zap")
    ("Zhuang" . "za")
    ("Zulu" . "zu"))
  "*Association list giving team codes from team names.
This is used for generating a submission file name for the 'M' command.
If a string instead of an alist, it is a team code to use unconditionnally."
  :type 'sexp
  :group 'k-po)

(defcustom k-po-gzip-uuencode-command "gzip -9 | uuencode -m"
  "*The filter to use for preparing a mail invoice of the PO file.
Normally \"gzip -9 | uuencode -m\", remove the -9 for lesser compression,
or remove the -m if you are not using the GNU version of `uuencode'."
  :type 'string
  :group 'k-po)

(defvar k-po-subedit-mode-syntax-table
  (copy-syntax-table text-mode-syntax-table)
  "Syntax table used while in PO mode.")

;;; Buffer local variables.

;; The following block of declarations has the main purpose of avoiding
;; byte compiler warnings.  It also introduces some documentation for
;; each of these variables, all meant to be local to PO mode buffers.

;; Flag telling that MODE-LINE-STRING should be displayed.  See 'Window'
;; page below.  Exceptionally, this variable is local to *all* buffers.
(defvar-local k-po-mode-flag nil)

;; The current entry extends from START-OF-ENTRY to END-OF-ENTRY, it
;; includes preceding whitespace and excludes following whitespace.  The
;; start of keyword lines are START-OF-MSGID and START-OF-MSGSTR.
;; ENTRY-TYPE classifies the entry.
(defvar-local k-po-start-of-entry nil)
(defvar-local k-po-start-of-msgctxt nil) ; = k-po-start-of-msgid if there is no msgctxt
(defvar-local k-po-start-of-msgid nil)
(defvar-local k-po-start-of-msgid_plural nil) ; = nil if there is no msgid_plural
(defvar-local k-po-start-of-msgstr-block nil)
(defvar-local k-po-start-of-msgstr-form nil)
(defvar-local k-po-end-of-msgstr-form nil)
(defvar-local k-po-end-of-entry nil)
(defvar-local k-po-entry-type nil)

;; A few counters are usefully shown in the Emacs mode line.
(defvar-local k-po-translated-counter nil)
(defvar-local k-po-fuzzy-counter nil)
(defvar-local k-po-untranslated-counter nil)
(defvar-local k-po-obsolete-counter nil)
(defvar-local k-po-mode-line-string nil)

;; PO mode keeps track of fields being edited, for one given field should
;; have one editing buffer at most, and for exiting a PO buffer properly
;; should offer to close all pending edits.  Variable EDITED-FIELDS holds an
;; an list of "slots" of the form: (ENTRY-MARKER EDIT-BUFFER OVERLAY-INFO).
;; To allow simultaneous edition of the comment and the msgstr of an entry,
;; ENTRY-MARKER points to the msgid line if a comment is being edited, or to
;; the msgstr line if the msgstr is being edited.  EDIT-BUFFER is the
;; temporary Emacs buffer used to edit the string.  OVERLAY-INFO, when not
;; nil, holds an overlay (or if overlays are not supported, a cons of two
;; markers) for this msgid string which became highlighted for the edit.
(defvar-local k-po-edited-fields nil)

;; We maintain a set of movable pointers for returning to entries.
(defvar-local k-po-marker-stack nil)

;; SEARCH path contains a list of directories where files may be found,
;; in a format suitable for read completion.  Each directory includes
;; its trailing slash.  PO mode starts with "./" and "../".
(defvar-local k-po-search-path nil)

;; The following variables are meaningful only when REFERENCE-CHECK
;; is identical to START-OF-ENTRY, else they should be recomputed.
;; REFERENCE-ALIST contains all known references for the current
;; entry, each list element is (PROMPT FILE LINE nil), where PROMPT may
;; be used for completing read, FILE is a string and LINE is a number.
;; REFERENCE-CURSOR is a cycling cursor into REFERENCE-ALIST.
(defvar-local k-po-reference-alist nil)
(defvar-local k-po-reference-cursor nil)
(defvar-local k-po-reference-check nil)

;; The following variables are for marking translatable strings in program
;; sources.  KEYWORDS is the list of keywords for marking translatable
;; strings, kept in a format suitable for reading with completion.
;; STRING-CONTENTS holds the value of the most recent string found in sources,
;; and when it is not nil, then STRING-BUFFER, STRING-START and STRING-END
;; describe where it is.  MARKING-OVERLAY, if not 'nil', holds the overlay
;; which highlight the last found string; for older Emacses, it holds the cons
;; of two markers around the highlighted region.
(defvar-local k-po-keywords nil)
(defvar-local k-po-string-contents nil)
(defvar-local k-po-string-buffer nil)
(defvar-local k-po-string-start nil)
(defvar-local k-po-string-end nil)
(defvar-local k-po-marking-overlay nil)

(provide 'k-po-vars)

;;; k-po-vars.el ends here
