;;; k-po-mode.el --- A po-mode fork that hopefully works better for me -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (f "0.20.0"))
;; Homepage: homepage
;; Keywords: po languages


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; I don't know why po-mode from
;; https://github.com/emacsmirror/po-mode/ gave me a bunch of errors,
;; so instead of figuring it out I'm just going to patch it so that it
;; works.

;;; Code:

(require 'f)

(require 'k-po-extract)
(require 'k-po-entry)
(require 'k-po-sidebar)
(require 'k-po-memory)


;;; Emacs portability matters - part I.
;;; Here is the minimum for customization to work.  See part II.

;;; Customisation.

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
Possible values are 'highlight', 'modeline', 'secondary-selection',
'region', and 'underline'.
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
or remove the -m if you are not using the GNU version of 'uuencode'."
  :type 'string
  :group 'k-po)

(defvar k-po-subedit-mode-syntax-table
  (copy-syntax-table text-mode-syntax-table)
  "Syntax table used while in PO mode.")


;; Handle portable highlighting.  Code has been adapted (OK... stolen! :-)
;; from 'ispell.el'.

(defun k-po-create-overlay ()
  "Create and return a deleted overlay structure.
The variable 'k-po-highlight-face' selects the face to use for highlighting."
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face k-po-highlight-face)
    ;; The fun thing is that a deleted overlay retains its face, and is
    ;; movable.
    (delete-overlay overlay)
    overlay))

(defun k-po-highlight (overlay start end &optional buffer)
  "Use OVERLAY to highlight the string from START to END.
If limits are not relative to the current buffer, use optional BUFFER."
  (move-overlay overlay start end (or buffer (current-buffer))))

(defun k-po-dehighlight (overlay)
  "Display normally the last string which OVERLAY highlighted.
The current buffer should be in PO mode, when this function is called."
  (delete-overlay overlay))

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

;;; PO mode variables and constants (usually not to customize).

(defconst k-po-help-display-string
  "\
PO Mode Summary           Next Previous            Miscellaneous
*: Later, /: Docum        n    p    Any type       .     Redisplay
                          t    T    Translated
Moving around             f    F    Fuzzy          ?, h  This help
<    First if any         o    O    Obsolete       =     Current index
>    Last if any          u    U    Untranslated   0     Other window
/SPC Auto select                                   V     Validate
                          Msgstr Comments          M     Mail officially
Modifying entries         RET  #    Call editor
TAB   Remove fuzzy mark   k    K    Kill to        E     Edit out full
DEL   Fuzzy or fade out   w    W    Copy to        Q     Forceful quit
LFD   Init with msgid     y    Y    Yank from      q     Confirm and quit

gettext Keyword Marking                            Position Stack
,    Find next string     Compendiums              m  Mark and push current
M-,  Mark translatable    *c    To compendium      r  Pop and return
M-.  Change mark, mark    *M-C  Select, save       x  Exchange current/top

Program Sources           Auxiliary Files          Lexicography
s    Cycle reference      a    Cycle file          *l    Lookup translation
M-s  Select reference     C-c C-a  Select file     *M-l  Add/edit translation
S    Consider path        A    Consider PO file    *L    Consider lexicon
M-S  Ignore path          M-A  Ignore PO file      *M-L  Ignore lexicon
"
  "Help page for PO mode.")

(defconst k-po-mode-menu-layout
  `("PO"
    ("Moving around"
     ["Auto select" k-po-auto-select-entry
      :help "Jump to next interesting entry"]
     "---"
     ;; Forward
     ["Any next" k-po-next-entry
      :help "Jump to next entry"]
     ["Next translated" k-po-next-translated-entry
      :help "Jump to next translated entry"]
     ["Next fuzzy" k-po-next-fuzzy-entry
      :help "Jump to next fuzzy entry"]
     ["Next obsolete" k-po-next-obsolete-entry
      :help "Jump to next obsolete entry"]
     ["Next untranslated" k-po-next-untranslated-entry
      :help "Jump to next untranslated entry"]
     ["Last file entry" k-po-last-entry
      :help "Jump to last entry"]
     "---"
     ;; Backward
     ["Any previous" k-po-previous-entry
      :help "Jump to previous entry"]
     ["Previous translated" k-po-previous-translated-entry
      :help "Jump to previous translated entry"]
     ["Previous fuzzy" k-po-previous-fuzzy-entry
      :help "Jump to previous fuzzy entry"]
     ["Previous obsolete" k-po-previous-obsolete-entry
      :help "Jump to previous obsolete entry"]
     ["Previous untranslated" k-po-previous-untranslated-entry
      :help "Jump to previous untranslated entry"]
     ["First file entry" k-po-first-entry
      :help "Jump to first entry"]
     "---"
     ;; "Position stack"
     ["Mark and push current" k-po-push-location
      :help "Remember current location"]
     ["Pop and return" k-po-pop-location
      :help "Jump to last remembered location and forget about it"]
     ["Exchange current/top" k-po-exchange-location
      :help "Jump to last remembered location and remember current location"]
     "---"
     ["Redisplay" k-po-display-current-entry
      :help "Make current entry properly visible"]
     ["Current index" k-po-statistics
      :help "Statistical info on current translation file"])
    ("Modifying entries"
     ;; "Msgstr"
     ["Edit msgstr" k-po-edit-msgstr
      :help "Edit current translation"]
     ["Ediff and merge msgstr" k-po-edit-msgstr-and-ediff
      :help "Call `ediff' on current translation for merging"]
     ["Cut msgstr" k-po-kill-msgstr
      :help "Cut (kill) current translation"]
     ["Copy msgstr" k-po-kill-ring-save-msgstr
      :help "Copy current translation"]
     ["Paste msgstr" k-po-yank-msgstr
      :help "Paste (yank) text most recently cut/copied translation"]
     "---"
     ;; "Comments"
     ["Edit comment" k-po-edit-comment
      :help "Edit current comment"]
     ["Ediff and merge comment" k-po-edit-comment-and-ediff
      :help "Call `ediff' on current comment for merging"]
     ["Cut comment" k-po-kill-comment
      :help "Cut (kill) current comment"]
     ["Copy comment" k-po-kill-ring-save-comment
      :help "Copy current translation"]
     ["Paste comment" k-po-yank-comment
      :help "Paste (yank) text most recently cut/copied"]
     "---"
     ["Remove fuzzy mark" k-po-unfuzzy
      :help "Remove \"#, fuzzy\""]
     ["Fuzzy or fade out" k-po-fade-out-entry
      :help "Set current entry fuzzy, or if already fuzzy delete it"]
     ["Init with msgid" k-po-msgid-to-msgstr
      :help "Initialize or replace current translation with the original message"])
    ("Other files"
     ["Other window" k-po-other-window
      :help "Select other window; if necessay split current frame"]
     "---"
     ;; "Program sources"
     ["Cycle reference in source file" k-po-cycle-source-reference t]
     ["Select reference" k-po-select-source-reference t]
     ["Consider path" k-po-consider-source-path t]
     ["Ignore path" k-po-ignore-source-path t]
     ;; "---"
     ;; ;; "Compendiums"
     ;; ["To add entry to compendium" k-po-save-entry nil]
     ;; ["Select from compendium, save" k-po-select-and-save-entry nil]
     "---"
     ;; "Auxiliary files"
     ["Cycle through auxilicary file" k-po-cycle-auxiliary t]
     ["Select auxilicary file" k-po-select-auxiliary t]
     ["Consider as auxilicary file" k-po-consider-as-auxiliary t]
     ["Ignore as auxilicary file" k-po-ignore-as-auxiliary t]
     ;; "---"
     ;; ;; "Lexicography"
     ;; ["Lookup translation" k-po-lookup-lexicons nil]
     ;; ["Add/edit translation" k-po-edit-lexicon-entry nil]
     ;; ["Consider lexicon" k-po-consider-lexicon-file nil]
     ;; ["Ignore lexicon" k-po-ignore-lexicon-file nil])
     "---"
     "Source marking"
     ["Find first string" (k-po-tags-search '(nil)) t]
     ["Prefer keyword" (k-po-select-mark-and-mark '(nil)) t]
     ["Find next string" k-po-tags-search t]
     ["Mark preferred" k-po-mark-translatable t]
     ["Mark with keyword" k-po-select-mark-and-mark t]
     "---"
     ["Validate" k-po-validate
      :help "Check validity of current translation file using `msgfmt'"]
     ["Mail officially" k-po-send-mail
      :help "Send current translation file to the Translation Robot by mail"]
     ["Edit out full" k-po-edit-out-full
      :help "Leave PO mode to edit translation file using fundamental mode"]
     "---"
     ["Forceful quit" k-po-quit
      :help "Close (kill) current translation file without saving"]
     ["Soft quit" k-po-confirm-and-quit
      :help "Save current translation file, than close (kill) it"])))


(defconst k-po-subedit-mode-menu-layout
  `("PO-Edit"
    ["Ediff and merge translation variants" k-po-subedit-ediff
      :help "Call `ediff' for merging variants"]
    ["Cycle through auxiliary files" k-po-subedit-cycle-auxiliary t]
    "---"
    ["Abort edit" k-po-subedit-abort
     :help "Don't change the translation"]
    ["Exit edit" k-po-subedit-exit
     :help "Use this text as the translation and close current edit buffer"]))

(defconst k-po-subedit-message
  "Type 'C-c C-c' once done, or 'C-c C-k' to abort edit"
  "Message to post in the minibuffer when an edit buffer is displayed.")

(defvar k-po-auxiliary-list nil
  "List of auxiliary PO files, in completing read format.")

(defvar k-po-auxiliary-cursor nil
  "Cursor into the 'k-po-auxiliary-list'.")

(defvar k-po-compose-mail-function
  (let ((functions '(compose-mail-other-window
                     message-mail-other-window
                     compose-mail
                     message-mail))
        result)
    (while (and (not result) functions)
      (if (fboundp (car functions))
          (setq result (car functions))
        (setq functions (cdr functions))))
    (cond (result)
          ((fboundp 'mail-other-window)
           (function (lambda (to subject)
                       (mail-other-window nil to subject))))
          ((fboundp 'mail)
           (function (lambda (to subject)
                       (mail nil to subject))))
          (t (function (lambda (to subject)
                         (error "I do not know how to mail to '%s'" to))))))
  "Function to start composing an electronic message.")

(defvar k-po-any-previous-msgctxt-regexp
  "^#\\(~\\)?|[ \t]*msgctxt.*\n\\(#\\(~\\)?|[ \t]*\".*\n\\)*"
  "Regexp matching a whole #| msgctxt field, whether obsolete or not.")

(defvar k-po-any-previous-msgid-regexp
  "^#\\(~\\)?|[ \t]*msgid.*\n\\(#\\(~\\)?|[ \t]*\".*\n\\)*"
  "Regexp matching a whole #| msgid field, whether obsolete or not.")

(defvar k-po-any-previous-msgid_plural-regexp
  "^#\\(~\\)?|[ \t]*msgid_plural.*\n\\(#\\(~\\)?|[ \t]*\".*\n\\)*"
  "Regexp matching a whole #| msgid_plural field, whether obsolete or not.")

(defvar k-po-any-msgctxt-msgid-regexp
  "^\\(#~[ \t]*\\)?msg\\(ctxt\\|id\\).*\n\\(\\(#~[ \t]*\\)?\".*\n\\)*"
  "Regexp matching a whole msgctxt or msgid field, whether obsolete or not.")

(defvar k-po-any-msgid-regexp
  "^\\(#~[ \t]*\\)?msgid.*\n\\(\\(#~[ \t]*\\)?\".*\n\\)*"
  "Regexp matching a whole msgid field, whether obsolete or not.")

(defvar k-po-any-msgid_plural-regexp
  "^\\(#~[ \t]*\\)?msgid_plural.*\n\\(\\(#~[ \t]*\\)?\".*\n\\)*"
  "Regexp matching a whole msgid_plural field, whether obsolete or not.")

(defvar k-po-any-msgstr-block-regexp
  "^\\(#~[ \t]*\\)?msgstr\\([ \t]\\|\\[0\\]\\).*\n\\(\\(#~[ \t]*\\)?\".*\n\\)*\\(\\(#~[ \t]*\\)?msgstr\\[[0-9]\\].*\n\\(\\(#~[ \t]*\\)?\".*\n\\)*\\)*"
  "Regexp matching a whole msgstr or msgstr[] field, whether obsolete or not.")

(defvar k-po-any-msgstr-form-regexp
  ;; "^\\(#~[ \t]*\\)?msgstr.*\n\\(\\(#~[ \t]*\\)?\".*\n\\)*"
  "^\\(#~[ \t]*\\)?msgstr\\(\\[[0-9]\\]\\)?.*\n\\(\\(#~[ \t]*\\)?\".*\n\\)*"
  "Regexp matching just one msgstr or msgstr[] field, whether obsolete or not.")

(defvar k-po-msgstr-idx-keyword-regexp
  "^\\(#~[ \t]*\\)?msgstr\\[[0-9]\\]"
  "Regexp matching an indexed msgstr keyword, whether obsolete or not.")

(defvar k-po-msgfmt-program "msgfmt"
  "Path to msgfmt program from GNU gettext package.")

;; Font lock based highlighting code.
(defconst k-po-font-lock-keywords
  '(
    ("^# .*\\|^#[:,]?" . font-lock-comment-face)
    ("^#:\\(.*\\)" 1 font-lock-reference-face)
    ("^#,\\(.*\\)" 1 font-lock-function-name-face)
    ("^\\(\\(msg\\(ctxt\\|id\\(_plural\\)?\\|str\\(\\[[0-9]\\]\\)?\\)\\) \\)?\"\\|\"$"
     . font-lock-keyword-face)
    ("\\\\.\\|%[$-.0-9hjltuzL]*[a-zA-Z]" . font-lock-variable-name-face))
  "Additional expressions to highlight in PO mode.")

;; Old activator for 'font lock'.  Is it still useful?  I don't think so.
;;(if (boundp 'font-lock-keywords)
;;    (put 'k-po-mode 'font-lock-keywords 'k-po-font-lock-keywords))

;; 'hilit19' based highlighting code has been disabled, as most probably
;; nobody really needs it (it also generates ugly byte-compiler warnings).
;;
;;(if (fboundp 'hilit-set-mode-patterns)
;;    (hilit-set-mode-patterns 'k-po-mode
;;                             '(("^# .*\\|^#$" nil comment)
;;                               ("^#[.,:].*" nil include)
;;                               ("^\\(msgid\\|msgstr\\) *\"" nil keyword)
;;                               ("^\"\\|\"$" nil keyword))))

;;; Mode activation.

(defvar k-po-mode-abbrev-table nil
  "Abbrev table used while in PO mode.")
(define-abbrev-table 'k-po-mode-abbrev-table ())

(defvar k-po-mode-map
  (let ((k-po-mode-map (make-sparse-keymap)))
    (define-key k-po-mode-map (kbd "C-c C-i")   #'k-po-unfuzzy)
    (define-key k-po-mode-map (kbd "C-c C-j")   #'k-po-msgid-to-msgstr)
    (define-key k-po-mode-map (kbd "C-c C-m")   #'k-po-edit-msgstr)
    (define-key k-po-mode-map (kbd "C-c SPC")   #'k-po-auto-select-entry)
    (define-key k-po-mode-map (kbd "C-c #")     #'k-po-edit-comment)
    (define-key k-po-mode-map (kbd "C-c ,")     #'k-po-tags-search)
    (define-key k-po-mode-map (kbd "C-c .")     #'k-po-display-current-entry)
    (define-key k-po-mode-map (kbd "C-c <")     #'k-po-first-entry)
    (define-key k-po-mode-map (kbd "C-c =")     #'k-po-statistics)
    (define-key k-po-mode-map (kbd "C-c >")     #'k-po-last-entry)
    (define-key k-po-mode-map (kbd "C-c a")     #'k-po-cycle-auxiliary)
    ;; (define-key k-po-mode-map (kbd "C-c c")  #'k-po-save-entry)
    (define-key k-po-mode-map (kbd "C-c f")     #'k-po-next-fuzzy-entry)
    (define-key k-po-mode-map (kbd "C-c k")     #'k-po-kill-msgstr)
    (define-key k-po-mode-map (kbd "C-c l")     #'k-po-sidebar-toggle)
    (define-key k-po-mode-map (kbd "C-c m")     #'k-po-push-location)
    (define-key k-po-mode-map (kbd "C-c n")     #'k-po-next-entry)
    (define-key k-po-mode-map (kbd "C-c o")     #'k-po-next-obsolete-entry)
    (define-key k-po-mode-map (kbd "C-c p")     #'k-po-previous-entry)
    (define-key k-po-mode-map (kbd "C-c q")     #'k-po-confirm-and-quit)
    (define-key k-po-mode-map (kbd "C-c r")     #'k-po-pop-location)
    (define-key k-po-mode-map (kbd "C-c s")     #'k-po-cycle-source-reference)
    (define-key k-po-mode-map (kbd "C-c t")     #'k-po-next-translated-entry)
    (define-key k-po-mode-map (kbd "C-c u")     #'k-po-next-untranslated-entry)
    (define-key k-po-mode-map (kbd "C-c w")     #'k-po-kill-ring-save-msgstr)
    (define-key k-po-mode-map (kbd "C-c x")     #'k-po-exchange-location)
    (define-key k-po-mode-map (kbd "C-c y")     #'k-po-yank-msgstr)
    (define-key k-po-mode-map (kbd "C-c A")     #'k-po-consider-as-auxiliary)
    (define-key k-po-mode-map (kbd "C-c E")     #'k-po-edit-out-full)
    (define-key k-po-mode-map (kbd "C-c F")     #'k-po-previous-fuzzy-entry)
    (define-key k-po-mode-map (kbd "C-c K")     #'k-po-kill-comment)
    ;; (define-key k-po-mode-map (kbd "C-c L")  #'k-po-consider-lexicon-file)
    (define-key k-po-mode-map (kbd "C-c M")     #'k-po-send-mail)
    (define-key k-po-mode-map (kbd "C-c O")     #'k-po-previous-obsolete-entry)
    (define-key k-po-mode-map (kbd "C-c T")     #'k-po-previous-translated-entry)
    (define-key k-po-mode-map (kbd "C-c U")     #'k-po-previous-untranslated-entry)
    (define-key k-po-mode-map (kbd "C-c Q")     #'k-po-quit)
    (define-key k-po-mode-map (kbd "C-c S")     #'k-po-consider-source-path)
    (define-key k-po-mode-map (kbd "C-c V")     #'k-po-validate)
    (define-key k-po-mode-map (kbd "C-c W")     #'k-po-kill-ring-save-comment)
    (define-key k-po-mode-map (kbd "C-c Y")     #'k-po-yank-comment)
    (define-key k-po-mode-map (kbd "C-c 0")     #'k-po-other-window)
    (define-key k-po-mode-map (kbd "C-c DEL")   #'k-po-fade-out-entry)
    (define-key k-po-mode-map (kbd "C-c C-a")   #'k-po-select-auxiliary)
    (define-key k-po-mode-map (kbd "C-c C-e")   #'k-po-edit-msgstr-and-ediff)
    (define-key k-po-mode-map (kbd "C-c C-#")   #'k-po-edit-comment-and-ediff)
    (define-key k-po-mode-map (kbd "C-c C-C")   #'k-po-edit-comment-and-ediff)
    (define-key k-po-mode-map (kbd "M-,")       #'k-po-mark-translatable)
    (define-key k-po-mode-map (kbd "M-.")       #'k-po-select-mark-and-mark)
    ;; (define-key k-po-mode-map (kbd "M-c")    #'k-po-select-and-save-entry)
    ;; (define-key k-po-mode-map (kbd "M-l")    #'k-po-edit-lexicon-entry)
    (define-key k-po-mode-map (kbd "M-s")       #'k-po-select-source-reference)
    (define-key k-po-mode-map (kbd "M-A")       #'k-po-ignore-as-auxiliary)
    ;; (define-key k-po-mode-map (kbd "M-L")    #'k-po-ignore-lexicon-file)
    (define-key k-po-mode-map (kbd "M-S")       #'k-po-ignore-source-path)
    k-po-mode-map)
  "Keymap for PO mode.")

;;;###autoload
(define-derived-mode k-po-mode fundamental-mode
  "PO"
  "Major mode for translators to edit PO files.

Special commands:
\\{k-po-mode-map}
Turning on PO mode calls the value of the variable `k-po-mode-hook',
if that value is non-nil.  Behaviour may be adjusted through some variables,
all reachable through \\[customize], in group `Emacs.Editing.I18n.K-po'."
  (when (fboundp 'easy-menu-define)
    (easy-menu-define k-po-mode-menu k-po-mode-map "" k-po-mode-menu-layout))
  (setq-local font-lock-defaults '(k-po-font-lock-keywords t))

  (setq k-po-mode-flag t)

  (k-po-check-file-header)
  (k-po-compute-counters nil)
  (k-po-memory--insert-current-file)

  (setq-local k-po-edited-fields nil)
  (setq-local k-po-marker-stack nil)
  (setq-local k-po-search-path '(("./") ("../")))

  (setq-local k-po-reference-alist nil)
  (setq-local k-po-reference-cursor nil)
  (setq-local k-po-reference-check 0)

  (setq-local k-po-keywords '(("gettext") ("gettext_noop") ("_") ("N_")))
  (setq-local k-po-string-contents nil)
  (setq-local k-po-string-buffer nil)
  (setq-local k-po-string-start nil)
  (setq-local k-po-string-end nil)
  (setq-local k-po-marking-overlay (k-po-create-overlay))

  (add-hook 'post-command-hook #'k-po-sidebar--post-command-h nil t)
  (add-hook 'write-contents-functions #'k-po-replace-revision-date))

(defvar k-po-subedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'k-po-subedit-cycle-auxiliary)
    (define-key map (kbd "C-c C-c") #'k-po-subedit-exit)
    (define-key map (kbd "C-c C-'") #'k-po-subedit-exit)
    (define-key map (kbd "C-c C-e") #'k-po-subedit-ediff)
    (define-key map (kbd "C-c C-k") #'k-po-subedit-abort)
    map)
  "Keymap while editing a PO mode entry (or the full PO file).")


;;; Window management.

(defvar k-po-mode-line-entry '(k-po-mode-flag ("  " k-po-mode-line-string))
  "Mode line format entry displaying MODE-LINE-STRING.")

;; Insert MODE-LINE-ENTRY in mode line, but on first load only.
(or (member k-po-mode-line-entry mode-line-format)
    ;; mode-line-format usually contains global-mode-string, but some
    ;; people customize this variable. As a last resort, append at the end.
    (let ((prev-entry (or (member 'global-mode-string mode-line-format)
                          (member "   " mode-line-format)
                          (last mode-line-format))))
      (setcdr prev-entry (cons k-po-mode-line-entry (cdr prev-entry)))))

(defun k-po-update-mode-line-string ()
  "Compute a new statistics string to display in mode line."
  (setq k-po-mode-line-string
        (concat (format "%dt" k-po-translated-counter)
                (if (> k-po-fuzzy-counter 0)
                    (format "+%df" k-po-fuzzy-counter))
                (if (> k-po-untranslated-counter 0)
                    (format "+%du" k-po-untranslated-counter))
                (if (> k-po-obsolete-counter 0)
                    (format "+%do" k-po-obsolete-counter))))
  (force-mode-line-update))

(defun k-po-type-counter ()
  "Return the symbol name of the counter appropriate for the current entry."
  (cond ((eq k-po-entry-type 'obsolete) 'k-po-obsolete-counter)
        ((eq k-po-entry-type 'fuzzy) 'k-po-fuzzy-counter)
        ((eq k-po-entry-type 'translated) 'k-po-translated-counter)
        ((eq k-po-entry-type 'untranslated) 'k-po-untranslated-counter)
        (t (error "Unknown entry type"))))

(defun k-po-decrease-type-counter ()
  "Decrease the counter corresponding to the nature of the current entry."
  (let ((counter (k-po-type-counter)))
    (set counter (1- (eval counter)))))

(defun k-po-increase-type-counter ()
  "Increase the counter corresponding to the nature of the current entry.
Then, update the mode line counters."
  (let ((counter (k-po-type-counter)))
    (set counter (1+ (eval counter))))
  (k-po-update-mode-line-string))

;; Avoid byte compiler warnings.
(defvar k-po-fuzzy-regexp)
(defvar k-po-untranslated-regexp)

(defun k-po-map-entries (func &optional reporter)
  "Call FUNC for each entry in the buffer.
If REPORTER is non-nil, it is ticked using
`progress-reporter-update' with the point position after each
iteration, and finalized using `progress-reporter-done' afterwards."
  (save-excursion
    (goto-char (point-min))
    (funcall func (k-po-current-entry))
    (when (re-search-forward "^msgid" nil t)
      (while (and (re-search-forward k-po-any-msgstr-block-regexp nil t)
                  (not (eobp)))
        (funcall func (k-po-current-entry))
        (when reporter
          (progress-reporter-update reporter (point))))
      (when reporter
        (progress-reporter-done reporter)))))

(defun k-po-compute-counters (echo)
  "Prepare counters for mode line display.  If ECHO, also echo entry position."
  (setq k-po-translated-counter 0
        k-po-fuzzy-counter 0
        k-po-untranslated-counter 0
        k-po-obsolete-counter 0)
  (let ((position 0) (total 0) current here)
    ;; FIXME 'here' looks obsolete / 2001-08-23 03:54:26 CEST -ke-
    (save-excursion
      (setq current (k-po-entry-msgstr-block-start (k-po-current-entry)))
      (goto-char (point-min))
      ;; While counting, skip the header entry, for consistency with msgfmt.
      (let ((entry (k-po-current-entry)))
        (when (string-equal (k-po-entry-msgid entry) "")
          (goto-char (k-po-entry-end entry))))
      (when (re-search-forward "^msgid" (point-max) t)
        ;; Start counting
        (while (re-search-forward k-po-any-msgstr-block-regexp nil t)
          (and (= (% total 20) 0)
               (if echo
                   (message "Position %d/%d" position total)
                 (message "Position %d" total)))
          (setq here (point))
          (goto-char (match-beginning 0))
          (setq total (1+ total))
          (and echo (eq (point) current) (setq position total))
          (cond ((eq (following-char) ?#)
                 (setq k-po-obsolete-counter (1+ k-po-obsolete-counter)))
                ((looking-at k-po-untranslated-regexp)
                 (setq k-po-untranslated-counter (1+ k-po-untranslated-counter)))
                (t (setq k-po-translated-counter (1+ k-po-translated-counter))))
          (goto-char here))

        ;; Make another pass just for the fuzzy entries, kind of kludgey.
        ;; FIXME: Counts will be wrong if untranslated entries are fuzzy, yet
        ;; this should not normally happen.
        (goto-char (point-min))
        (while (re-search-forward k-po-fuzzy-regexp nil t)
          (setq k-po-fuzzy-counter (1+ k-po-fuzzy-counter)))
        (setq k-po-translated-counter (- k-po-translated-counter k-po-fuzzy-counter))))

    ;; Push the results out.
    (if echo
        (message "\
Position %d/%d; %d translated, %d fuzzy, %d untranslated, %d obsolete"
                 position total k-po-translated-counter k-po-fuzzy-counter
                 k-po-untranslated-counter k-po-obsolete-counter)
      (message "")))
  (k-po-update-mode-line-string))

(defun k-po-redisplay ()
  "Redisplay the current entry."
  ;; FIXME: Should try to fit the whole entry on the window.  If this is not
  ;; possible, should try to fit the comment and the msgid.  Otherwise,
  ;; should try to fit the msgid.  Else, the first line of the msgid should
  ;; be at the top of the window.
  (goto-char k-po-start-of-msgid))

(defun k-po-other-window ()
  "Get the cursor into another window, out of PO mode."
  (interactive)
  (if (one-window-p t)
      (progn
        (split-window)
        (switch-to-buffer (other-buffer)))
    (other-window 1)))

;;; Processing the PO file header entry.

(defun k-po-check-file-header ()
  "Create a missing PO mode file header, or replace an oldish one.
Can be customized with the `k-po-auto-update-file-header' variable."
  (if (or (eq k-po-auto-update-file-header t)
          (and (eq k-po-auto-update-file-header 'ask)
               (y-or-n-p "May I update the PO Header Entry? ")))
      (save-excursion
        (save-restriction
          (widen) ; in case of a narrowed view to the buffer
          (let (insert-flag end-of-header)
            (goto-char (point-min))
            (if (re-search-forward k-po-any-msgstr-block-regexp nil t)
                (progn
                  ;; There is at least one entry.
                  (goto-char (match-beginning 0))
                  (forward-line -1)
                  (setq end-of-header (match-end 0))
                  (if (looking-at "msgid \"\"\n")
                      ;; There is indeed a PO file header.
                      (if (re-search-forward "\n\"PO-Revision-Date: "
                                             end-of-header t)
                          nil
                        ;; This is an oldish header.  Replace it all.
                        (goto-char end-of-header)
                        (while (> (point) (point-min))
                          (forward-line -1)
                          (insert "#~ ")
                          (beginning-of-line))
                        (beginning-of-line)
                        (setq insert-flag t))
                    ;; The first entry is not a PO file header, insert one.
                    (setq insert-flag t)))
              ;; Not a single entry found.
              (setq insert-flag t))
            (goto-char (point-min))
            (if insert-flag
                (progn
                  (insert k-po-default-file-header)
                  (if (not (eobp))
                      (insert "\n")))))))
    (message "PO Header Entry was not updated...")))

(defun k-po-replace-revision-date ()
  "Replace the revision date by current time in the PO file header."
  (if (fboundp 'format-time-string)
      (if (or (eq k-po-auto-replace-revision-date t)
              (and (eq k-po-auto-replace-revision-date 'ask)
                   (y-or-n-p "May I set PO-Revision-Date? ")))
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "^\"PO-Revision-Date:.*" nil t)
                (let* ((time (current-time))
                       (seconds (or (car (current-time-zone time)) 0))
                       (minutes (/ (abs seconds) 60))
                       (zone (format "%c%02d%02d"
                                     (if (< seconds 0) ?- ?+)
                                     (/ minutes 60)
                                     (% minutes 60))))
                  (replace-match
                   (concat "\"PO-Revision-Date: "
                           (format-time-string "%Y-%m-%d %H:%M" time)
                           zone "\\n\"")
                   t t))))
        (message ""))
    (message "PO-Revision-Date should be adjusted..."))
  ;; Return nil to indicate that the buffer has not yet been saved.
  nil)

;;; Handling span of entry, entry type and entry attributes.

;; TODO: Return an entry object, not this.
(defun k-po-find-span-of-entry ()
  "Find the extent of the PO file entry where the cursor is.
Set variables k-po-start-of-entry, k-po-start-of-msgctxt, k-po-start-of-msgid,
k-po-start-of-msgid_plural, k-po-start-of-msgstr-block, k-po-end-of-entry, and
k-po-entry-type to meaningful values. k-po-entry-type may be set to: obsolete,
fuzzy, untranslated, or translated."
  (let ((here (point)))
    (if (re-search-backward k-po-any-msgstr-block-regexp nil t)
        (progn
          ;; After a backward match, (match-end 0) will not extend
          ;; beyond point, in case point was *inside* the regexp.  We
          ;; need a dependable (match-end 0), so we redo the match in
          ;; the forward direction.
          (re-search-forward k-po-any-msgstr-block-regexp)
          (if (<= (match-end 0) here)
              (progn
                ;; We most probably found the msgstr of the previous
                ;; entry.  The current entry then starts just after
                ;; its end, save this information just in case.
                (setq k-po-start-of-entry (match-end 0))
                ;; However, it is also possible that we are located in
                ;; the crumb after the last entry in the file.  If
                ;; yes, we know the middle and end of last PO entry.
                (setq k-po-start-of-msgstr-block (match-beginning 0)
                      k-po-end-of-entry (match-end 0))
                (if (re-search-forward k-po-any-msgstr-block-regexp nil t)
                    (progn
                      ;; We definitely were not in the crumb.
                      (setq k-po-start-of-msgstr-block (match-beginning 0)
                            k-po-end-of-entry (match-end 0)))
                  ;; We were in the crumb.  The start of the last PO
                  ;; file entry is the end of the previous msgstr if
                  ;; any, or else, the beginning of the file.
                  (goto-char k-po-start-of-msgstr-block)
                  (setq k-po-start-of-entry
                        (if (re-search-backward k-po-any-msgstr-block-regexp nil t)
                            (match-end 0)
                          (point-min)))))
            ;; The cursor was inside msgstr of the current entry.
            (setq k-po-start-of-msgstr-block (match-beginning 0)
                  k-po-end-of-entry (match-end 0))
            ;; The start of this entry is the end of the previous
            ;; msgstr if any, or else, the beginning of the file.
            (goto-char k-po-start-of-msgstr-block)
            (setq k-po-start-of-entry
                  (if (re-search-backward k-po-any-msgstr-block-regexp nil t)
                      (match-end 0)
                    (point-min)))))
      ;; The cursor was before msgstr in the first entry in the file.
      (setq k-po-start-of-entry (point-min))
      (goto-char k-po-start-of-entry)
      ;; There is at least the PO file header, so this should match.
      (re-search-forward k-po-any-msgstr-block-regexp)
      (setq k-po-start-of-msgstr-block (match-beginning 0)
            k-po-end-of-entry (match-end 0)))
    ;; Find start of msgid.
    (goto-char k-po-start-of-entry)
    (re-search-forward k-po-any-msgctxt-msgid-regexp)
    (setq k-po-start-of-msgctxt (match-beginning 0))
    (goto-char k-po-start-of-entry)
    (re-search-forward k-po-any-msgid-regexp)
    (setq k-po-start-of-msgid (match-beginning 0))
    (save-excursion
      (goto-char k-po-start-of-msgid)
      (setq k-po-start-of-msgid_plural
            (if (re-search-forward k-po-any-msgid_plural-regexp
                                   k-po-start-of-msgstr-block t)
                (match-beginning 0)
              nil)))
    (save-excursion
      (when (>= here k-po-start-of-msgstr-block)
        ;; point was somewhere inside of msgstr*
        (goto-char here)
        (end-of-line)
        (re-search-backward "^\\(#~[ \t]*\\)?msgstr"))
      ;; Detect the boundaries of the msgstr we are interested in.
      (re-search-forward k-po-any-msgstr-form-regexp)
      (setq k-po-start-of-msgstr-form (match-beginning 0)
            k-po-end-of-msgstr-form (match-end 0)))
    ;; Classify the entry.
    (setq k-po-entry-type
          (if (eq (following-char) ?#)
              'obsolete
            (goto-char k-po-start-of-entry)
            (if (re-search-forward k-po-fuzzy-regexp k-po-start-of-msgctxt t)
                'fuzzy
              (goto-char k-po-start-of-msgstr-block)
              (if (looking-at k-po-untranslated-regexp)
                  'untranslated
                'translated))))
    ;; Put the cursor back where it was.
    (goto-char here)))

(defun k-po-current-entry ()
  "Return the current entry at point, as an entry object."
  (k-po-find-span-of-entry)
  (k-po--entry-from-vars))

(defun k-po-add-attribute (name)
  "Add attribute NAME to the current entry, unless it is already there."
  (save-excursion
    (goto-char k-po-start-of-entry)
    (if (re-search-forward "\n#, .*" k-po-start-of-msgctxt t)
        (save-restriction
          (narrow-to-region (match-beginning 0) (match-end 0))
          (goto-char (point-min))
          (if (re-search-forward (concat "\\b" name "\\b") nil t)
              nil
            (goto-char (point-max))
            (insert ", " name)))
      (skip-chars-forward "\n")
      (while (eq (following-char) ?#)
        (forward-line 1))
      (insert "#, " name "\n"))))

(defun k-po-delete-attribute (name)
  "Delete attribute NAME from the current entry, if any."
  (save-excursion
    (goto-char k-po-start-of-entry)
    (if (re-search-forward "\n#, .*" k-po-start-of-msgctxt t)
        (save-restriction
          (narrow-to-region (match-beginning 0) (match-end 0))
          (goto-char (point-min))
          (if (re-search-forward
               (concat "\\(\n#, " name "$\\|, " name "$\\| " name ",\\)")
               nil t)
              (replace-match "" t t))))))

;;; Entry positionning.

(defun k-po-say-location-depth ()
  "Tell how many entries in the entry location stack."
  (let ((depth (length k-po-marker-stack)))
    (cond ((= depth 0) (message "Empty location stack"))
          ((= depth 1) (message "One entry in location stack"))
          (t (message "%d entries in location stack" depth)))))

(defun k-po-push-location ()
  "Stack the location of the current entry, for later return."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (save-excursion
      (goto-char (k-po-entry-msgid-start entry))
      (setq k-po-marker-stack (cons (point-marker) k-po-marker-stack)))
    (k-po-say-location-depth)))

(defun k-po-pop-location ()
  "Unstack a saved location, and return to the corresponding entry."
  (interactive)
  (if k-po-marker-stack
      (progn
        (goto-char (car k-po-marker-stack))
        (setq k-po-marker-stack (cdr k-po-marker-stack))
        (k-po-display-current-entry)
        (k-po-say-location-depth))
    (error "The entry location stack is empty")))

(defun k-po-exchange-location ()
  "Exchange the location of the current entry with the top of stack."
  (interactive)
  (unless k-po-marker-stack
    (error "The entry location stack is empty"))
  (let ((entry (k-po-current-entry)))
    (goto-char (k-po-entry-msgid-start entry))
    (let ((location (point-marker)))
      (goto-char (car k-po-marker-stack))
      (setq k-po-marker-stack (cons location (cdr k-po-marker-stack))))
    (k-po-display-current-entry)
    (k-po-say-location-depth)))

(defun k-po-display-current-entry ()
  "Display the current entry."
  (interactive)
  (k-po-find-span-of-entry)
  (k-po-redisplay))

(defun k-po-first-entry-with-regexp (regexp)
  "Display the first entry in the file which msgstr matches REGEXP."
  (let ((here (point)))
    (goto-char (point-min))
    (if (re-search-forward regexp nil t)
        (progn
          (goto-char (match-beginning 0))
          (k-po-display-current-entry))
      (goto-char here)
      (error "There is no such entry"))))

(defun k-po-last-entry-with-regexp (regexp)
  "Display the last entry in the file which msgstr matches REGEXP."
  (let ((here (point)))
    (goto-char (point-max))
    (if (re-search-backward regexp nil t)
        (k-po-display-current-entry)
      (goto-char here)
      (error "There is no such entry"))))

(defun k-po-next-entry-with-regexp (regexp wrap)
  "Display the entry following the current entry which msgstr matches REGEXP.
If WRAP is not nil, the search may wrap around the buffer."
  (let ((entry (k-po-current-entry))
        (here (point)))
    (goto-char (oref entry end))
    (if (re-search-forward regexp nil t)
        (progn
          (goto-char (match-beginning 0))
          (k-po-display-current-entry))
      (if (and wrap
               (progn
                 (goto-char (point-min))
                 (re-search-forward regexp (oref entry start) t)))
          (progn
            (goto-char (match-beginning 0))
            (k-po-display-current-entry)
            (message "Wrapping around the buffer"))
        (goto-char here)
        (error "There is no such entry")))))

(defun k-po-previous-entry-with-regexp (regexp wrap)
  "Redisplay the entry preceding the current entry which msgstr matches REGEXP.
If WRAP is not nil, the search may wrap around the buffer."
  (let ((entry (k-po-current-entry))
        (here (point)))
    (goto-char (oref entry start))
    (if (re-search-backward regexp nil t)
        (k-po-display-current-entry)
      (if (and wrap
               (progn
                 (goto-char (point-max))
                 (re-search-backward regexp (oref entry end) t)))
          (progn
            (k-po-display-current-entry)
            (message "Wrapping around the buffer"))
        (goto-char here)
        (error "There is no such entry")))))

;; Any entries.

(defun k-po-jump-to-entry (source target)
  "Jump to the entry whose msgid is SOURCE and msgstr is TARGET."
  (let ((found
         (catch 'found
           (k-po-map-entries
            (lambda (entry)
              (when (and (equal source (k-po-entry-msgid entry))
                         (equal target (k-po-entry-msgstr entry)))
                (throw 'found (point)))))
           ;; If we reach this point, we haven't found it.
           nil)))
    (when found (goto-char found))))

(defun k-po-first-entry ()
  "Display the first entry."
  (interactive)
  (k-po-first-entry-with-regexp k-po-any-msgstr-block-regexp))

(defun k-po-last-entry ()
  "Display the last entry."
  (interactive)
  (k-po-last-entry-with-regexp k-po-any-msgstr-block-regexp))

(defun k-po-next-entry ()
  "Display the entry following the current entry."
  (interactive)
  (k-po-next-entry-with-regexp k-po-any-msgstr-block-regexp nil))

(defun k-po-previous-entry ()
  "Display the entry preceding the current entry."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-any-msgstr-block-regexp nil))

;; Untranslated entries.

(defvar k-po-after-entry-regexp
  "\\(\\'\\|\\(#[ \t]*\\)?$\\)"
  "Regexp which should be true after a full msgstr string matched.")

(defvar k-po-untranslated-regexp
  (concat "^msgstr\\(\\[[0-9]\\]\\)?[ \t]*\"\"\n" k-po-after-entry-regexp)
  "Regexp matching a whole msgstr field, but only if active and empty.")

(defun k-po-next-untranslated-entry ()
  "Find the next untranslated entry, wrapping around if necessary."
  (interactive)
  (k-po-next-entry-with-regexp k-po-untranslated-regexp t))

(defun k-po-previous-untranslated-entry ()
  "Find the previous untranslated entry, wrapping around if necessary."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-untranslated-regexp t))

(defun k-po-msgid-to-msgstr ()
  "Use another window to edit msgstr reinitialized with msgid."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (if (or (k-po-entry-type? entry 'untranslated)
            (k-po-entry-type? entry 'obsolete)
            (prog1 (y-or-n-p "Really lose previous translation? ")
              (message "")))
        ;; In an entry with plural forms, use the msgid_plural string,
        ;; as it is more general than the msgid string.
        (if (k-po-set-msgstr-form (or (k-po-entry-msgid_plural entry)
                                      (k-po-entry-msgid entry)))
            (k-po-maybe-delete-previous-untranslated)))))

;; Obsolete entries.

(defvar k-po-obsolete-msgstr-regexp
  "^#~[ \t]*msgstr.*\n\\(#~[ \t]*\".*\n\\)*"
  "Regexp matching a whole msgstr field of an obsolete entry.")

(defun k-po-next-obsolete-entry ()
  "Find the next obsolete entry, wrapping around if necessary."
  (interactive)
  (k-po-next-entry-with-regexp k-po-obsolete-msgstr-regexp t))

(defun k-po-previous-obsolete-entry ()
  "Find the previous obsolete entry, wrapping around if necessary."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-obsolete-msgstr-regexp t))

;; Fuzzy entries.

(defvar k-po-fuzzy-regexp "^#, .*fuzzy"
  "Regexp matching the fuzzy marker.
It is inserted by msgmerge for translations which does not match
exactly.")

(defun k-po-next-fuzzy-entry ()
  "Find the next fuzzy entry, wrapping around if necessary."
  (interactive)
  (k-po-next-entry-with-regexp k-po-fuzzy-regexp t))

(defun k-po-previous-fuzzy-entry ()
  "Find the next fuzzy entry, wrapping around if necessary."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-fuzzy-regexp t))

(defun k-po-toggle-fuzzy ()
  "Toggle fuzzy for the current entry."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (if (k-po-entry-type? entry 'fuzzy)
        (k-po-delete-attribute "fuzzy")
      (k-po-add-attribute "fuzzy"))))

(defun k-po-unfuzzy ()
  "Remove the fuzzy attribute for the current entry."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (when (k-po-entry-type? entry 'fuzzy)
      (k-po-decrease-type-counter)
      (k-po-delete-attribute "fuzzy")
      (k-po-maybe-delete-previous-untranslated)
      (k-po-display-current-entry)
      (k-po-increase-type-counter))
    (when k-po-auto-select-on-unfuzzy
      (k-po-auto-select-entry))
    (k-po-update-mode-line-string)))

;; Translated entries.

(defun k-po-next-translated-entry ()
  "Find the next translated entry, wrapping around if necessary."
  (interactive)
  (if (= k-po-translated-counter 0)
      (error "There is no such entry")
    (k-po-next-entry-with-regexp k-po-any-msgstr-block-regexp t)
    (let ((entry (k-po-current-entry)))
      (while (not (k-po-entry-type? entry 'translated))
        (k-po-next-entry-with-regexp k-po-any-msgstr-block-regexp t)
        (setq entry (k-po-current-entry))))))

(defun k-po-previous-translated-entry ()
  "Find the previous translated entry, wrapping around if necessary."
  (interactive)
  (if (= k-po-translated-counter 0)
      (error "There is no such entry")
    (k-po-previous-entry-with-regexp k-po-any-msgstr-block-regexp t)
    (let ((entry (k-po-current-entry)))
      (while (not (k-po-entry-type? entry 'translated))
        (k-po-previous-entry-with-regexp k-po-any-msgstr-block-regexp t)
        (setq entry (k-po-current-entry))))))

;; Auto-selection feature.

(defun k-po-auto-select-entry ()
  "Select the next entry having the same type as the current one.
If none, wrap from the beginning of the buffer with another type,
going from untranslated to fuzzy, and from fuzzy to obsolete.
Plain translated entries are always disregarded unless there are
no entries of the other types."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (goto-char (oref entry end))
    (if (and (= k-po-untranslated-counter 0)
             (= k-po-fuzzy-counter 0)
             (= k-po-obsolete-counter 0))
        ;; All entries are plain translated.  Next entry will do, or
        ;; wrap around if there is none.
        (if (re-search-forward k-po-any-msgstr-block-regexp nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-min)))
      ;; If over a translated entry, look for an untranslated one first.
      ;; Else, look for an entry of the same type first.
      (let ((goal (if (k-po-entry-type? entry 'translated)
                      'untranslated
                    (oref entry type))))
        (while goal
          ;; Find an untranslated entry, or wrap up for a fuzzy entry.
          (if (eq goal 'untranslated)
              (if (and (> k-po-untranslated-counter 0)
                       (re-search-forward k-po-untranslated-regexp nil t))
                  (progn
                    (goto-char (match-beginning 0))
                    (setq goal nil))
                (goto-char (point-min))
                (setq goal 'fuzzy)))
          ;; Find a fuzzy entry, or wrap up for an obsolete entry.
          (if (eq goal 'fuzzy)
              (if (and (> k-po-fuzzy-counter 0)
                       (re-search-forward k-po-fuzzy-regexp nil t))
                  (progn
                    (goto-char (match-beginning 0))
                    (setq goal nil))
                (goto-char (point-min))
                (setq goal 'obsolete)))
          ;; Find an obsolete entry, or wrap up for an untranslated entry.
          (if (eq goal 'obsolete)
              (if (and (> k-po-obsolete-counter 0)
                       (re-search-forward k-po-obsolete-msgstr-regexp nil t))
                  (progn
                    (goto-char (match-beginning 0))
                    (setq goal nil))
                (goto-char (point-min))
                (setq goal 'untranslated)))))))
  ;; Display this entry nicely.
  (k-po-display-current-entry))

;;; Killing and yanking fields.

(defun k-po-get-msgid ()
  "Extract and return the unquoted msgid string."
  (k-po-extract-unquoted (current-buffer)
                         k-po-start-of-msgid
                         (or k-po-start-of-msgid_plural
                             k-po-start-of-msgstr-block)))

(defun k-po-get-msgid_plural ()
  "Extract and return the unquoted msgid_plural string.
Return nil if it is not present."
  (when k-po-start-of-msgid_plural
    (k-po-extract-unquoted (current-buffer)
                           k-po-start-of-msgid_plural
                           k-po-start-of-msgstr-block)))

(defun k-po-get-msgstr-flavor ()
  "Helper function to detect msgstr and msgstr[] variants.
Returns one of \"msgstr\" or \"msgstr[i]\" for some i."
  (save-excursion
    (goto-char k-po-start-of-msgstr-form)
    (re-search-forward "^\\(#~[ \t]*\\)?\\(msgstr\\(\\[[0-9]\\]\\)?\\)")
    (match-string 2)))

(defun k-po-get-msgstr-form ()
  "Extract and return the unquoted msgstr string."
  (let ((string (k-po-extract-unquoted (current-buffer)
                                     k-po-start-of-msgstr-form
                                     k-po-end-of-msgstr-form)))
    string))

(defun k-po-set-msgid (func)
  "Replace the current msgid, using FUNC to get a string.
Calling FUNC should insert the wanted string in the current
buffer. If FUNC is itself a string, then this string is used for
insertion. The string is properly requoted before the replacement
occurs.

Return nil if the buffer has not been modified, for if the new msgid
described by CALL is merely identical to the msgid already in place."
  (let ((string (k-po-call-requoted func "msgid" (eq k-po-entry-type 'obsolete))))
    (save-excursion
      (goto-char k-po-start-of-entry)
      (re-search-forward k-po-any-msgid-regexp k-po-start-of-msgstr-block)
      (and (not (string-equal (match-string-no-properties 0) string))
           (replace-match string t t)
           (goto-char k-po-start-of-msgid)
           (k-po-find-span-of-entry)
           t))))

(defun k-po-set-msgstr-form (func)
  "Replace the current msgstr or msgstr[], using FUNC to get a string.
Calling FUNC should insert the wanted string in the current
buffer. If FUNC is itself a string, then this string is used for
insertion. The string is properly requoted before the replacement
occurs.

Return nil if the buffer has not been modified, for if the new msgstr
described by FORM is merely identical to the msgstr already in place."
  (let ((string (k-po-call-requoted func
                                    (k-po-get-msgstr-flavor)
                                    (eq k-po-entry-type 'obsolete))))
    (save-excursion
      (goto-char k-po-start-of-msgstr-form)
      (re-search-forward k-po-any-msgstr-form-regexp k-po-end-of-msgstr-form)
      (and (not (string-equal (match-string-no-properties 0) string))
           (k-po-decrease-type-counter)
           (replace-match string t t)
           (goto-char k-po-start-of-msgid)
           (k-po-find-span-of-entry)
           (k-po-increase-type-counter)
           t))))

(defun k-po-kill-ring-save-msgstr ()
  "Push the msgstr string from current entry on the kill ring."
  (interactive)
  (let ((string (k-po-entry-msgstr (k-po-current-entry))))
    (kill-new string)
    string))

(defun k-po-kill-msgstr ()
  "Empty the msgstr string from current entry, pushing it on the kill ring."
  (interactive)
  (k-po-kill-ring-save-msgstr)
  (when (k-po-set-msgstr-form "")
    (k-po-maybe-delete-previous-untranslated)))

(defun k-po-yank-msgstr ()
  "Replace the current msgstr string by the top of the kill ring."
  (interactive)
  (k-po-find-span-of-entry)
  (when (k-po-set-msgstr-form
         (lambda ()
           (if (eq last-command 'yank)
               (yank-pop 1)
             (yank))))
    (k-po-maybe-delete-previous-untranslated))
  (setq this-command 'yank))

(defun k-po-fade-out-entry ()
  "Mark an active entry as fuzzy; obsolete a fuzzy or untranslated entry;
or completely delete an obsolete entry, saving its msgstr on the kill ring."
  (interactive)
  (k-po-find-span-of-entry)

  (cond ((eq k-po-entry-type 'translated)
         (k-po-decrease-type-counter)
         (k-po-add-attribute "fuzzy")
         (k-po-display-current-entry)
         (k-po-increase-type-counter))

        ((or (eq k-po-entry-type 'fuzzy)
             (eq k-po-entry-type 'untranslated))
         (if (y-or-n-p "Should I really obsolete this entry? ")
             (progn
               (k-po-decrease-type-counter)
               (save-excursion
                 (save-restriction
                   (narrow-to-region k-po-start-of-entry k-po-end-of-entry)
                   (goto-char (point-min))
                   (skip-chars-forward "\n")
                   (while (not (eobp))
                     (insert "#~ ")
                     (search-forward "\n"))))
               (k-po-display-current-entry)
               (k-po-increase-type-counter)))
         (message ""))

        ((and (eq k-po-entry-type 'obsolete)
              (k-po-check-for-pending-edit k-po-start-of-msgid)
              (k-po-check-for-pending-edit k-po-start-of-msgstr-block))
         (k-po-decrease-type-counter)
         (k-po-update-mode-line-string)
         ;; TODO: Should save all msgstr forms here, not just one.
         (kill-new (k-po-get-msgstr-form))
         (delete-region k-po-start-of-entry k-po-end-of-entry)
         (goto-char k-po-start-of-entry)
         (if (re-search-forward k-po-any-msgstr-block-regexp nil t)
             (goto-char (match-beginning 0))
           (re-search-backward k-po-any-msgstr-block-regexp nil t))
         (k-po-display-current-entry)
         (message ""))))

;;; Killing and yanking comments.

(defvar k-po-comment-regexp
  "^\\(#\n\\|# .*\n\\)+"
  "Regexp matching the whole editable comment part of an entry.")

(defun k-po-get-comment (kill-flag)
  "Extract and return the editable comment string, uncommented.
If KILL-FLAG, then add the unquoted comment to the kill ring."
  (let ((buffer (current-buffer))
        (obsolete (eq k-po-entry-type 'obsolete)))
    (save-excursion
      (goto-char k-po-start-of-entry)
      (if (re-search-forward k-po-comment-regexp k-po-end-of-entry t)
          (with-temp-buffer
            (insert-buffer-substring buffer (match-beginning 0) (match-end 0))
            (goto-char (point-min))
            (while (not (eobp))
              (if (looking-at (if obsolete "#\\(\n\\| \\)" "# ?"))
                  (replace-match "" t t))
              (forward-line 1))
            (and kill-flag (copy-region-as-kill (point-min) (point-max)))
            (buffer-string))
        ""))))

(defun k-po-set-comment (form)
  "Using FORM to get a string, replace the current editable comment.
Evaluating FORM should insert the wanted string in the current buffer.
If FORM is itself a string, then this string is used for insertion.
The string is properly recommented before the replacement occurs."
  (let ((obsolete (eq k-po-entry-type 'obsolete))
        string)
    (with-temp-buffer
      (if (stringp form)
          (insert form)
        (push-mark)
        (eval form))
      (if (not (or (bobp) (= (preceding-char) ?\n)))
          (insert "\n"))
      (goto-char (point-min))
      (while (not (eobp))
        (insert (if (= (following-char) ?\n) "#" "# "))
        (search-forward "\n"))
      (setq string (buffer-string)))
    (goto-char k-po-start-of-entry)
    (if (re-search-forward k-po-comment-regexp k-po-end-of-entry t)
        (if (not (string-equal (match-string-no-properties 0) string))
            (replace-match string t t))
      (skip-chars-forward " \t\n")
      (insert string)))
  (k-po-display-current-entry))

(defun k-po-kill-ring-save-comment ()
  "Push the msgstr string from current entry on the kill ring."
  (interactive)
  (k-po-find-span-of-entry)
  (k-po-get-comment t))

(defun k-po-kill-comment ()
  "Empty the msgstr string from current entry, pushing it on the kill ring."
  (interactive)
  (k-po-kill-ring-save-comment)
  (k-po-set-comment "")
  (k-po-redisplay))

(defun k-po-yank-comment ()
  "Replace the current comment string by the top of the kill ring."
  (interactive)
  (k-po-find-span-of-entry)
  (k-po-set-comment (if (eq last-command 'yank) '(yank-pop 1) '(yank)))
  (setq this-command 'yank)
  (k-po-redisplay))

;;; Deleting the "previous untranslated" comment.

(defun k-po-previous-untranslated-region-for (regexp)
  "Return the list of previous untranslated regions (at most one) for the
given regular expression REGEXP."
  (save-excursion
    (goto-char k-po-start-of-entry)
    (if (re-search-forward regexp k-po-start-of-msgctxt t)
        (list (cons (copy-marker (match-beginning 0))
                    (copy-marker (match-end 0))))
      nil)))

(defun k-po-previous-untranslated-regions ()
  "Return the list of previous untranslated regions in the current entry."
  (append (k-po-previous-untranslated-region-for k-po-any-previous-msgctxt-regexp)
          (k-po-previous-untranslated-region-for k-po-any-previous-msgid-regexp)
          (k-po-previous-untranslated-region-for k-po-any-previous-msgid_plural-regexp)))

(defun k-po-delete-previous-untranslated ()
  "Delete the previous msgctxt, msgid, msgid_plural fields (marked as #|
comments) from the current entry."
  (interactive)
  (k-po-find-span-of-entry)
  (dolist (region (k-po-previous-untranslated-regions))
    (delete-region (car region) (cdr region)))
  (k-po-redisplay))

(defun k-po-maybe-delete-previous-untranslated ()
  "Delete the previous msgctxt, msgid, msgid_plural fields (marked as #|
comments) from the current entry, if the user gives the permission."
  (k-po-find-span-of-entry)
  (let ((previous-regions (k-po-previous-untranslated-regions)))
    (if previous-regions
        (if (or (eq k-po-auto-delete-previous-msgid t)
                (and (eq k-po-auto-delete-previous-msgid 'ask)
                     (let ((overlays nil))
                       (unwind-protect
                           (progn
                             (setq overlays
                                   (mapcar (function
                                            (lambda (region)
                                              (let ((overlay (k-po-create-overlay)))
                                                (k-po-highlight overlay (car region) (cdr region))
                                                overlay)))
                                           previous-regions))
                             ;; Scroll, to show the previous-regions.
                             (goto-char (car (car previous-regions)))
                             (prog1 (y-or-n-p "Delete previous msgid comments? ")
                               (message "")))
                         (mapc 'k-po-dehighlight overlays)))))
            (dolist (region previous-regions)
              (delete-region (car region) (cdr region)))))))

;;; Editing management and submode.

;; In a string edit buffer, BACK-POINTER points to one of the slots of the
;; list EDITED-FIELDS kept in the PO buffer.  See its description elsewhere.
;; Reminder: slots have the form (ENTRY-MARKER EDIT-BUFFER OVERLAY-INFO).

(defvar k-po-subedit-back-pointer)

(defun k-po-clean-out-killed-edits ()
  "From EDITED-FIELDS, clean out any edit having a killed edit buffer."
  (let ((cursor k-po-edited-fields))
    (while cursor
      (let ((slot (car cursor)))
        (setq cursor (cdr cursor))
        (if (buffer-name (nth 1 slot))
            nil
          (let ((overlay (nth 2 slot)))
            (and overlay (k-po-dehighlight overlay)))
          (setq k-po-edited-fields (delete slot k-po-edited-fields)))))))

(defun k-po-check-all-pending-edits ()
  "Resume any pending edit.  Return nil if some remains."
  (k-po-clean-out-killed-edits)
  (or (null k-po-edited-fields)
      (let ((slot (car k-po-edited-fields)))
        (goto-char (nth 0 slot))
        (pop-to-buffer (nth 1 slot))
        (message k-po-subedit-message)
        nil)))

(defun k-po-check-for-pending-edit (position)
  "Resume any pending edit at POSITION.  Return nil if such edit exists."
  (k-po-clean-out-killed-edits)
  (let ((marker (make-marker)))
    (set-marker marker position)
    (let ((slot (assoc marker k-po-edited-fields)))
      (if slot
          (progn
            (goto-char marker)
            (pop-to-buffer (nth 1 slot))
            (message k-po-subedit-message)))
      (not slot))))

(defun k-po-edit-out-full ()
  "Get out of PO mode, leaving PO file buffer in fundamental mode."
  (interactive)
  (if (k-po-check-all-pending-edits)
      ;; Don't ask the user for confirmation, since he has explicitly asked
      ;; for it.
      (progn
        (fundamental-mode)
        (message "Type 'M-x k-po-mode RET' once done"))))

(defun k-po-ediff-quit ()
  "Quit ediff and exit `recursive-edit'."
  (interactive)
  (ediff-quit t)
  (exit-recursive-edit))

(add-hook 'ediff-keymap-setup-hook
          #'(lambda ()
              (define-key ediff-mode-map "Q" 'k-po-ediff-quit)))

;; Avoid byte compiler warnings.
(defvar k-po-subedit--entry-buffer)
(defvar-local k-po-subedit--reused? nil
  "Used to mark whether a subedit buffer is in a reused window.")

(defun k-po-ediff-buffers-exit-recursive (b1 b2 oldbuf end)
  "Ediff buffer B1 and B2, pop back to OLDBUF and replace the old variants.
This function will delete the first two variants in OLDBUF, call
`ediff-buffers' to compare both strings and replace the two variants in
OLDBUF with the contents of B2.
Once done kill B1 and B2.

For more info cf. `k-po-subedit-ediff'."
  (ediff-buffers b1 b2)
  (recursive-edit)
  (pop-to-buffer oldbuf)
  (delete-region (point-min) end)
  (insert-buffer-substring b2)
  (mapc 'kill-buffer `(,b1 ,b2))
  (display-buffer k-po-subedit--entry-buffer t))

(defun k-po-subedit-ediff ()
  "Edit the subedit buffer using `ediff'.
`k-po-subedit-ediff' calls `k-po-ediff-buffers-exit-recursive' to
edit translation variants side by side if they are actually
different; if variants are equal just delete the first one.

`msgcat' is able to produce those variants; every variant is marked with:

#-#-#-#-#  file name reference  #-#-#-#-#

Put changes in second buffer.

When done with the `ediff' session press \\[exit-recursive-edit]
exit to `recursive-edit', or call \\[k-po-ediff-quit] (`Q') in
the ediff control panel."
  (interactive)
  (let* ((marker-regex "^#-#-#-#-#  \\(.*\\)  #-#-#-#-#\n")
         (buf1 " *k-po-msgstr-1") ; default if first marker is missing
         buf2 start-1 end-1 start-2 end-2
         (back-pointer k-po-subedit-back-pointer)
         (entry-marker (nth 0 back-pointer))
         (k-po-subedit--entry-buffer (marker-buffer entry-marker)))
    (goto-char (point-min))
    (if (looking-at marker-regex)
        (and (setq buf1 (match-string-no-properties 1))
             (forward-line 1)))
    (setq start-1 (point))
    (if (not (re-search-forward marker-regex (point-max) t))
        (error "Only 1 msgstr found")
      (setq buf2 (match-string-no-properties 1)
            end-1 (match-beginning 0))
      (let ((oldbuf (current-buffer)))
        (save-current-buffer
          (set-buffer (get-buffer-create
                       (generate-new-buffer-name buf1)))
          (erase-buffer)
          (insert-buffer-substring oldbuf start-1 end-1))

        (setq start-2 (point))
        (save-excursion
          ;; check for a third variant; if found ignore it
          (if (re-search-forward marker-regex (point-max) t)
              (setq end-2 (match-beginning 0))
            (setq end-2 (goto-char (1- (point-max))))))
        (save-current-buffer
          (set-buffer (get-buffer-create
                       (generate-new-buffer-name buf2)))
          (erase-buffer)
          (insert-buffer-substring oldbuf start-2 end-2))

        (if (not (string-equal (buffer-substring-no-properties start-1 end-1)
                               (buffer-substring-no-properties start-2 end-2)))
            (k-po-ediff-buffers-exit-recursive buf1 buf2 oldbuf end-2)
          (message "Variants are equal; delete %s" buf1)
          (forward-line -1)
          (delete-region (point-min) (point)))))))

(defun k-po-subedit-abort ()
  "Exit the subedit buffer, merely discarding its contents."
  (interactive)
  (let* ((edit-buffer (current-buffer))
         (back-pointer k-po-subedit-back-pointer)
         (entry-marker (nth 0 back-pointer))
         (overlay-info (nth 2 back-pointer))
         (k-po-subedit--entry-buffer (marker-buffer entry-marker)))
    (if (null k-po-subedit--entry-buffer)
        (error "Corresponding PO buffer does not exist anymore")
      (unless (or k-po-subedit--reused? (one-window-p))
        (delete-window))
      (switch-to-buffer k-po-subedit--entry-buffer)
      (goto-char entry-marker)
      (and overlay-info (k-po-dehighlight overlay-info))
      (kill-buffer edit-buffer)
      (setq k-po-edited-fields (delete back-pointer k-po-edited-fields)))))

(defun k-po-subedit-exit ()
  "Exit the subedit buffer, replacing the string in the PO buffer."
  (interactive)
  (goto-char (point-max))
  (skip-chars-backward " \t\n")
  (if (eq (preceding-char) ?<)
      (delete-region (1- (point)) (point-max)))
  (run-hooks 'k-po-subedit-exit-hook)
  (let ((str (buffer-string)))
    (k-po-subedit-abort)
    (k-po-find-span-of-entry)
    (cond ((= (point) k-po-start-of-msgid)
           (k-po-set-comment str)
           (k-po-redisplay))
          ((= (point) k-po-start-of-msgstr-form)
           (when (k-po-set-msgstr-form str)
             (k-po-maybe-delete-previous-untranslated)
             (if (and k-po-auto-fuzzy-on-edit
                      (eq k-po-entry-type 'translated))
                 (progn
                   (k-po-decrease-type-counter)
                   (k-po-add-attribute "fuzzy")
                   (k-po-display-current-entry)
                   (k-po-increase-type-counter)))))
          (t (debug)))))

(defun k-po-edit-string (string type expand-tabs)
  "Prepare a pop up buffer for editing STRING, which is of a given TYPE.
TYPE may be 'comment or 'msgstr.  If EXPAND-TABS, expand tabs to spaces.
Run functions on k-po-subedit-mode-hook."
  (let ((marker (make-marker)))
    (set-marker marker (cond ((eq type 'comment) k-po-start-of-msgid)
                             ((eq type 'msgstr) k-po-start-of-msgstr-form)))
    (if (k-po-check-for-pending-edit marker)
        (let ((edit-buffer (generate-new-buffer
                            (concat "*" (buffer-name) "*")))
              (edit-coding buffer-file-coding-system)
              (buffer (current-buffer))
              (window-count-before-switch (count-windows))
              overlay slot)
          (if (and (eq type 'msgstr))
              ;; ;; Try showing all of msgid in the upper window while editing.
              ;; (goto-char (1- k-po-start-of-msgstr-block))
              ;; (recenter -1)
              (save-excursion
                (goto-char k-po-start-of-entry)
                (re-search-forward k-po-any-msgid-regexp nil t)
                (let ((end (1- (match-end 0))))
                  (goto-char (match-beginning 0))
                  (re-search-forward "msgid +" nil t)
                  (setq overlay (k-po-create-overlay))
                  (k-po-highlight overlay (point) end buffer))))
          (setq slot (list marker edit-buffer overlay)
                k-po-edited-fields (cons slot k-po-edited-fields))
          (pop-to-buffer edit-buffer)
          (text-mode)
          (when (= window-count-before-switch (count-windows))
            (setq-local k-po-subedit--reused? t))
          (setq-local k-po-subedit-back-pointer slot)
          (setq-local indent-line-function
                      'indent-relative)
          (setq buffer-file-coding-system edit-coding)
          (setq local-abbrev-table k-po-mode-abbrev-table)
          (erase-buffer)
          (insert string)
          (goto-char (point-min))
          (and expand-tabs (setq indent-tabs-mode nil))
          (use-local-map k-po-subedit-mode-map)
          (if (fboundp 'easy-menu-define)
              (easy-menu-define k-po-subedit-mode-menu k-po-subedit-mode-map ""
                k-po-subedit-mode-menu-layout))
          (set-syntax-table k-po-subedit-mode-syntax-table)
          (run-hooks 'k-po-subedit-mode-hook)
          (message k-po-subedit-message)))))

(defun k-po-edit-comment ()
  "Use another window to edit the current translator comment."
  (interactive)
  (k-po-find-span-of-entry)
  (k-po-edit-string (k-po-get-comment nil) 'comment nil))

(defun k-po-edit-comment-and-ediff ()
  "Use `ediff' to edit the current translator comment.
This function calls `k-po-edit-msgstr' and `k-po-subedit-ediff'; for more info
read `k-po-subedit-ediff' documentation."
  (interactive)
  (k-po-edit-comment)
  (k-po-subedit-ediff))

(defun k-po-edit-msgstr ()
  "Use another window to edit the current msgstr."
  (interactive)
  (k-po-find-span-of-entry)
  (k-po-edit-string (if (and k-po-auto-edit-with-msgid
                             (eq k-po-entry-type 'untranslated))
                        (k-po-get-msgid)
                      (k-po-get-msgstr-form))
                    'msgstr
                    t))

(defun k-po-edit-msgstr-and-ediff ()
  "Use `ediff' to edit the current msgstr.
This function calls `k-po-edit-msgstr' and `k-po-subedit-ediff'; for more info
read `k-po-subedit-ediff' documentation."
  (interactive)
  (k-po-edit-msgstr)
  (k-po-subedit-ediff))

;;; String normalization and searching.

(defun k-po-normalize-old-style (explain)
  "Normalize old gettext style fields using K&R C multiline string syntax.
To minibuffer messages sent while normalizing, add the EXPLAIN string."
  (let ((here (point-marker))
        (counter 0))
    (goto-char (point-min))
    (message "Normalizing %d, %s" counter explain)
    (while (re-search-forward
            "\\(^#?[ \t]*msg\\(id\\|str\\)[ \t]*\"\\|[^\" \t][ \t]*\\)\\\\\n"
            nil t)
      (if (= (% counter 10) 0)
          (message "Normalizing %d, %s" counter explain))
      (replace-match "\\1\"\n\"" t nil)
      (setq counter (1+ counter)))
    (goto-char here)
    (message "Normalizing %d...done" counter)))

(defun k-po-normalize-field (field explain)
  "Normalize FIELD of all entries.  FIELD is either the symbol msgid or msgstr.
To minibuffer messages sent while normalizing, add the EXPLAIN string."
  (let ((here (point-marker))
        (counter 0))
    (goto-char (point-min))
    (while (re-search-forward k-po-any-msgstr-block-regexp nil t)
      (if (= (% counter 10) 0)
          (message "Normalizing %d, %s" counter explain))
      (goto-char (match-beginning 0))
      (k-po-find-span-of-entry)
      (cond ((eq field 'msgid) (k-po-set-msgid (k-po-get-msgid)))
            ((eq field 'msgstr) (k-po-set-msgstr-form (k-po-get-msgstr-form))))
      (goto-char k-po-end-of-entry)
      (setq counter (1+ counter)))
    (goto-char here)
    (message "Normalizing %d...done" counter)))

;; Normalize, but the British way! :-)
(defsubst k-po-normalise () (k-po-normalize))

(defun k-po-normalize ()
  "Normalize all entries in the PO file."
  (interactive)
  (k-po-normalize-old-style "pass 1/3")
  ;; FIXME: This cannot work: t and nil are not msgid and msgstr.
  (k-po-normalize-field t "pass 2/3")
  (k-po-normalize-field nil "pass 3/3")
  ;; The last PO file entry has just been processed.
  (if (not (= k-po-end-of-entry (point-max)))
      (kill-region k-po-end-of-entry (point-max)))
  ;; A bizarre format might have fooled the counters, so recompute
  ;; them to make sure their value is dependable.
  (k-po-compute-counters nil))

;;; Multiple PO files.

(defun k-po-show-auxiliary-list ()
  "Echo the current auxiliary list in the message area."
  (if k-po-auxiliary-list
      (let ((cursor k-po-auxiliary-cursor)
            string)
        (while cursor
          (setq string (concat string (if string " ") (car (car cursor)))
                cursor (cdr cursor)))
        (setq cursor k-po-auxiliary-list)
        (while (not (eq cursor k-po-auxiliary-cursor))
          (setq string (concat string (if string " ") (car (car cursor)))
                cursor (cdr cursor)))
        (message string))
    (message "No auxiliary files.")))

(defun k-po-consider-as-auxiliary ()
  "Add the current PO file to the list of auxiliary files."
  (interactive)
  (if (member (list buffer-file-name) k-po-auxiliary-list)
      nil
    (setq k-po-auxiliary-list
          (nconc k-po-auxiliary-list (list (list buffer-file-name))))
    (or k-po-auxiliary-cursor
        (setq k-po-auxiliary-cursor k-po-auxiliary-list)))
  (k-po-show-auxiliary-list))

(defun k-po-ignore-as-auxiliary ()
  "Delete the current PO file from the list of auxiliary files."
  (interactive)
  (setq k-po-auxiliary-list (delete (list buffer-file-name) k-po-auxiliary-list)
        k-po-auxiliary-cursor k-po-auxiliary-list)
  (k-po-show-auxiliary-list))

(defun k-po-seek-equivalent-translation (name string)
  "Search a PO file NAME for a 'msgid' STRING having a non-empty 'msgstr'.
STRING is the full quoted msgid field, including the 'msgid' keyword.  When
found, display the file over the current window, with the 'msgstr' field
possibly highlighted, the cursor at start of msgid, then return 't'.
Otherwise, move nothing, and just return 'nil'."
  (let ((current (current-buffer))
        (buffer (find-file-noselect name)))
    (set-buffer buffer)
    (let ((start (point))
          found)
      (goto-char (point-min))
      (while (and (not found) (search-forward string nil t))
        ;; Screen out longer 'msgid's.
        (if (looking-at "^msgstr ")
            (progn
              (k-po-find-span-of-entry)
              ;; Ignore an untranslated entry.
              (or (string-equal
                   (buffer-substring k-po-start-of-msgstr-block k-po-end-of-entry)
                   "msgstr \"\"\n")
                  (setq found t)))))
      (if found
          (progn
            (switch-to-buffer buffer)
            (k-po-find-span-of-entry)
            (goto-char k-po-start-of-entry)
            (re-search-forward k-po-any-msgstr-block-regexp nil t)
            (let ((end (1- (match-end 0))))
              (goto-char (match-beginning 0))
              (re-search-forward "msgstr +" nil t)
              ;; Just "borrow" the marking overlay.
              (k-po-highlight k-po-marking-overlay (point) end))
            (goto-char k-po-start-of-msgid))
        (goto-char start)
        (k-po-find-span-of-entry)
        (set-buffer current))
      found)))

(defun k-po-cycle-auxiliary ()
  "Select the next auxiliary file having an entry with same 'msgid'."
  (interactive)
  (k-po-find-span-of-entry)
  (if k-po-auxiliary-list
      (let ((string (buffer-substring k-po-start-of-msgid
                                      k-po-start-of-msgstr-block))
            (cursor k-po-auxiliary-cursor)
            found name)
        (while (and (not found) cursor)
          (setq name (car (car cursor)))
          (if (and (not (string-equal buffer-file-name name))
                   (k-po-seek-equivalent-translation name string))
              (setq found t
                    k-po-auxiliary-cursor cursor))
          (setq cursor (cdr cursor)))
        (setq cursor k-po-auxiliary-list)
        (while (and (not found) cursor)
          (setq name (car (car cursor)))
          (if (and (not (string-equal buffer-file-name name))
                   (k-po-seek-equivalent-translation name string))
              (setq found t
                    k-po-auxiliary-cursor cursor))
          (setq cursor (cdr cursor)))
        (or found (message "No other translation found"))
        found)))

(defun k-po-subedit-cycle-auxiliary ()
  "Cycle auxiliary file, but from the translation edit buffer."
  (interactive)
  (let* ((entry-marker (nth 0 k-po-subedit-back-pointer))
         (k-po-subedit--entry-buffer (marker-buffer entry-marker))
         (buffer (current-buffer)))
    (pop-to-buffer k-po-subedit--entry-buffer)
    (k-po-cycle-auxiliary)
    (pop-to-buffer buffer)))

(defun k-po-select-auxiliary ()
  "Select one of the available auxiliary files and locate an equivalent entry.
If an entry having the same 'msgid' cannot be found, merely select the file
without moving its cursor."
  (interactive)
  (k-po-find-span-of-entry)
  (if k-po-auxiliary-list
      (let ((string
              (buffer-substring k-po-start-of-msgid k-po-start-of-msgstr-block))
            (name (car (assoc (completing-read "Which auxiliary file? "
                                               k-po-auxiliary-list nil t)
                              k-po-auxiliary-list))))
        (k-po-consider-as-auxiliary)
        (or (k-po-seek-equivalent-translation name string)
            (find-file name)))))

;;; Original program sources as context.

(defun k-po-show-source-path ()
  "Echo the current source search path in the message area."
  (if k-po-search-path
      (let ((cursor k-po-search-path)
            string)
        (while cursor
          (setq string (concat string (if string " ") (car (car cursor)))
                cursor (cdr cursor)))
        (message string))
    (message "Empty source path.")))

(defun k-po-consider-source-path (directory)
  "Add a given DIRECTORY, requested interactively, to the source search path."
  (interactive "DDirectory for search path: ")
  (setq k-po-search-path (cons (list (if (string-match "/$" directory)
                                         directory
                                       (concat directory "/")))
                             k-po-search-path))
  (setq k-po-reference-check 0)
  (k-po-show-source-path))

(defun k-po-ignore-source-path ()
  "Delete a directory, selected with completion, from the source search path."
  (interactive)
  (setq k-po-search-path
        (delete (list (completing-read "Directory to remove? "
                                       k-po-search-path nil t))
                k-po-search-path))
  (setq k-po-reference-check 0)
  (k-po-show-source-path))

(defun k-po-ensure-source-references ()
  "Extract all references into a list, with paths resolved, if necessary."
  (k-po-find-span-of-entry)
  (if (= k-po-start-of-entry k-po-reference-check)
      nil
    (setq k-po-reference-alist nil)
    (save-excursion
      (goto-char k-po-start-of-entry)
      (if (re-search-forward "^#:" k-po-start-of-msgid t)
          (let (current name line path file)
            (while (looking-at "\\(\n#:\\)? *\\([^: ]*\\):\\([0-9]+\\)")
              (goto-char (match-end 0))
              (setq name (match-string-no-properties 2)
                    line (match-string-no-properties 3)
                    path k-po-search-path)
              (if (string-equal name "")
                  nil
                (while (and (not (file-exists-p
                                  (setq file (concat (car (car path)) name))))
                            path)
                  (setq path (cdr path)))
                (setq current (and path file)))
              (if current
                  (setq k-po-reference-alist
                        (cons (list (concat current ":" line)
                                    current
                                    (string-to-number line))
                              k-po-reference-alist)))))))
    (setq k-po-reference-alist (nreverse k-po-reference-alist)
          k-po-reference-cursor k-po-reference-alist
          k-po-reference-check k-po-start-of-entry)))

(defun k-po-show-source-context (triplet)
  "Show the source context given a TRIPLET which is (PROMPT FILE LINE)."
  (find-file-other-window (car (cdr triplet)))
  (goto-line (car (cdr (cdr triplet))))
  (other-window 1)
  (let ((maximum 0)
        position
        (cursor k-po-reference-alist))
    (while (not (eq triplet (car cursor)))
      (setq maximum (1+ maximum)
            cursor (cdr cursor)))
    (setq position (1+ maximum)
          k-po-reference-cursor cursor)
    (while cursor
      (setq maximum (1+ maximum)
            cursor (cdr cursor)))
    (message "Displaying %d/%d: \"%s\"" position maximum (car triplet))))

(defun k-po-cycle-source-reference ()
  "Display some source context for the current entry.
If the command is repeated many times in a row, cycle through contexts."
  (interactive)
  (k-po-ensure-source-references)
  (if k-po-reference-cursor
      (k-po-show-source-context
       (car (if (eq last-command 'k-po-cycle-source-reference)
                (or (cdr k-po-reference-cursor) k-po-reference-alist)
              k-po-reference-cursor)))
    (error "No resolved source references")))

(defun k-po-select-source-reference ()
  "Select one of the available source contexts for the current entry."
  (interactive)
  (k-po-ensure-source-references)
  (if k-po-reference-alist
      (k-po-show-source-context
       (assoc
        (completing-read "Which source context? " k-po-reference-alist nil t)
        k-po-reference-alist))
    (error "No resolved source references")))

;;; String marking in program sources, through TAGS table.

;; Globally defined within tags.el.
(defvar tags-loop-operate)
(defvar tags-loop-scan)

;; Locally set in each program source buffer.
(defvar k-po-find-string-function)
(defvar k-po-mark-string-function)

;; Dynamically set within k-po-tags-search for k-po-tags-loop-operate.
(defvar k-po-current-po-buffer)
(defvar k-po-current-po-keywords)

(defun k-po-tags-search (restart)
  "Find an unmarked translatable string through all files in tags table.
Disregard some simple strings which are most probably non-translatable.
With prefix argument, restart search at first file."
  (interactive "P")
  (require 'etags)
  ;; Ensure there is no highlighting, in case the search fails.
  (k-po-dehighlight k-po-marking-overlay)
  (setq k-po-string-contents nil)
  ;; Search for a string which might later be marked for translation.
  (let ((k-po-current-po-buffer (current-buffer))
        (k-po-current-po-keywords k-po-keywords))
    (pop-to-buffer k-po-string-buffer)
    (if (and (not restart)
             (eq (car tags-loop-operate) 'k-po-tags-loop-operate))
        ;; Continue last k-po-tags-search.
        (tags-loop-continue nil)
      ;; Start or restart k-po-tags-search all over.
      (setq tags-loop-scan '(k-po-tags-loop-scan)
            tags-loop-operate '(k-po-tags-loop-operate))
      (tags-loop-continue t))
    (select-window (get-buffer-window k-po-current-po-buffer)))
  (when k-po-string-contents
    (let ((window (selected-window))
          (buffer k-po-string-buffer)
          (start k-po-string-start)
          (end k-po-string-end))
      ;; Try to fit the string in the displayed part of its window.
      (select-window (get-buffer-window buffer))
      (goto-char start)
      (or (pos-visible-in-window-p start)
          (recenter '(nil)))
      (if (pos-visible-in-window-p end)
          (goto-char end)
        (goto-char end)
        (recenter -1))
      (select-window window)
      ;; Highlight the string as found.
      (k-po-highlight k-po-marking-overlay start end buffer))))

(defun k-po-tags-loop-scan ()
  "Decide if the current buffer is still interesting for PO mode strings."
  ;; We have little choice, here.  The major mode is needed to dispatch to the
  ;; proper scanner, so we declare all files as interesting, to force Emacs
  ;; tags module to revisit files fully.  k-po-tags-loop-operate sets point at
  ;; end of buffer when it is done with a file.
  (not (eobp)))

(defun k-po-tags-loop-operate ()
  "Find an acceptable tag in the current buffer, according to mode.
Disregard some simple strings which are most probably non-translatable."
  (let ((continue t)
        data)
    (while continue
      (setq data (apply k-po-find-string-function k-po-current-po-keywords nil))
      (if data
          ;; Push the string just found into a work buffer for study.
          (with-temp-buffer
            (insert (nth 0 data))
            (goto-char (point-min))
            ;; Accept if at least three letters in a row.
            (if (re-search-forward "[A-Za-z][A-Za-z][A-Za-z]" nil t)
                (setq continue nil)
              ;; Disregard if single letters or no letters at all.
              (if (re-search-forward "[A-Za-z][A-Za-z]" nil t)
                  ;; Here, we have two letters in a row, but never more.
                  ;; Accept only if more letters than punctuations.
                  (let ((total (buffer-size)))
                    (goto-char (point-min))
                    (while (re-search-forward "[A-Za-z]+" nil t)
                      (replace-match "" t t))
                    (if (< (* 2 (buffer-size)) total)
                        (setq continue nil))))))
        ;; No string left in this buffer.
        (setq continue nil)))
    (if data
        ;; Save information for marking functions.
        (let ((buffer (current-buffer)))
          (with-current-buffer k-po-current-po-buffer
            (setq k-po-string-contents (nth 0 data)
                  k-po-string-buffer buffer
                  k-po-string-start (nth 1 data)
                  k-po-string-end (nth 2 data))))
      (goto-char (point-max)))
    ;; If nothing was found, trigger scanning of next file.
    (not data)))

(defun k-po-mark-found-string (keyword)
  "Mark last found string in program sources as translatable, using KEYWORD."
  (unless k-po-string-contents
    (error "No such string"))
  (k-po-dehighlight k-po-marking-overlay)
  (let ((contents k-po-string-contents)
        (buffer k-po-string-buffer)
        (start k-po-string-start)
        (end k-po-string-end)
        line)
    ;; Mark string in program sources.
    (with-current-buffer buffer
      (setq line (count-lines (point-min) start))
      (apply k-po-mark-string-function start end keyword nil))
    ;; Add PO file entry.
    (goto-char (point-max))
    (insert "\n" (format "#: %s:%d\n"
                         (buffer-file-name k-po-string-buffer)
                         line))
    (save-excursion
      (insert (k-po-call-requoted contents "msgid" nil)
              "msgstr \"\"\n"))
    (setq k-po-untranslated-counter (1+ k-po-untranslated-counter))
    (k-po-update-mode-line-string)
    (setq k-po-string-contents nil)))

(defun k-po-mark-translatable ()
  "Mark last found string in program sources as translatable, using '_'."
  (interactive)
  (k-po-mark-found-string "_"))

(defun k-po-select-mark-and-mark (arg)
  "Mark last found string in program sources as translatable, ask for keyword,
using completion.  With prefix argument, just ask the name of a preferred
keyword for subsequent commands, also added to possible completions."
  (interactive "P")
  (if arg
      (let ((keyword (list (read-from-minibuffer "Keyword: "))))
        (setq k-po-keywords (cons keyword (delete keyword k-po-keywords))))
    (unless k-po-string-contents
      (error "No such string"))
    (let* ((default (car (car k-po-keywords)))
           (keyword (completing-read (format "Mark with keyword? [%s] "
                                             default)
                                     k-po-keywords nil t)))
      (if (string-equal keyword "") (setq keyword default))
      (k-po-mark-found-string keyword))))

;;; Unknown mode specifics.

(defvar-local k-po-find-string-function
  (cond ((equal major-mode 'awk-mode)
         #'k-po-find-awk-string)
        ((member major-mode '(c-mode c++-mode))
         #'k-po-find-c-string)
        ((equal major-mode 'emacs-lisp-mode)
         #'k-po-find-emacs-lisp-string)
        ((equal major-mode 'python-mode)
         #'k-po-find-python-string)
        ((and (equal major-mode 'sh-mode)
              (string-equal mode-line-process "[bash]"))
         #'k-po-find-bash-string)
        (t
         #'k-po-find-unknown-string)))
(defvar-local k-po-mark-string-function
  (cond ((equal major-mode 'awk-mode)
         #'k-po-mark-awk-string)
        ((member major-mode '(c-mode c++-mode))
         #'k-po-mark-c-string)
        ((equal major-mode 'emacs-lisp-mode)
         #'k-po-mark-emacs-lisp-string)
        ((equal major-mode 'python-mode)
         #'k-po-mark-python-string)
        ((and (equal major-mode 'sh-mode)
              (string-equal mode-line-process "[bash]"))
         #'k-po-mark-bash-string)
        (t
         #'k-po-mark-unknown-string)))

(defun k-po-find-unknown-string (keywords)
  "Dummy function to skip over a file, finding no string in it."
  nil)

(defun k-po-mark-unknown-string (start end keyword)
  "Dummy function to mark a given string.  May not be called."
  (error "Dummy function called"))

;;; Awk mode specifics.

(defun k-po-find-awk-string (keywords)
  "Find the next Awk string, excluding those marked by any of KEYWORDS.
Return (CONTENTS START END) for the found string, or nil if none found."
  (let (start end)
    (while (and (not start)
                (re-search-forward "[#/\"]" nil t))
      (cond ((= (preceding-char) ?#)
             ;; Disregard comments.
             (or (search-forward "\n" nil t)
                 (goto-char (point-max))))
            ((= (preceding-char) ?/)
             ;; Skip regular expressions.
             (while (not (= (following-char) ?/))
               (skip-chars-forward "^/\\\\")
               (if (= (following-char) ?\\) (forward-char 2)))
             (forward-char 1))
            ;; Else find the end of the string.
            (t (setq start (1- (point)))
               (while (not (= (following-char) ?\"))
                 (skip-chars-forward "^\"\\\\")
                 (if (= (following-char) ?\\) (forward-char 2)))
               (forward-char 1)
               (setq end (point))
               ;; Check before string either for underline, or for keyword
               ;; and opening parenthesis.
               (save-excursion
                 (goto-char start)
                 (cond ((= (preceding-char) ?_)
                        ;; Disregard already marked strings.
                        (setq start nil
                              end nil))
                       ((= (preceding-char) ?\()
                        (backward-char 1)
                        (let ((end-keyword (point)))
                          (skip-chars-backward "_A-Za-z0-9")
                          (if (member (list (buffer-substring-no-properties
                                             (point) end-keyword))
                                      keywords)
                              ;; Disregard already marked strings.
                              (setq start nil
                                    end nil)))))))))
    (and start end
         (list (k-po-extract-unquoted (current-buffer) start end) start end))))

(defun k-po-mark-awk-string (start end keyword)
  "Mark the Awk string, from START to END, with KEYWORD.
Leave point after marked string."
  (if (string-equal keyword "_")
      (progn
        (goto-char start)
        (insert "_")
        (goto-char (1+ end)))
    (goto-char end)
    (insert ")")
    (save-excursion
      (goto-char start)
      (insert keyword "("))))

;;; Bash mode specifics.

(defun k-po-find-bash-string (keywords)
  "Find the next unmarked Bash string.  KEYWORDS are merely ignored.
Return (CONTENTS START END) for the found string, or nil if none found."
  (let (start end)
    (while (and (not start)
                (re-search-forward "[#'\"]" nil t))
      (cond ((= (preceding-char) ?#)
             ;; Disregard comments.
             (or (search-forward "\n" nil t)
                 (goto-char (point-max))))
            ((= (preceding-char) ?')
             ;; Skip single quoted strings.
             (while (not (= (following-char) ?'))
               (skip-chars-forward "^'\\\\")
               (if (= (following-char) ?\\) (forward-char 2)))
             (forward-char 1))
            ;; Else find the end of the double quoted string.
            (t (setq start (1- (point)))
               (while (not (= (following-char) ?\"))
                 (skip-chars-forward "^\"\\\\")
                 (if (= (following-char) ?\\) (forward-char 2)))
               (forward-char 1)
               (setq end (point))
               ;; Check before string for dollar sign.
               (save-excursion
                 (goto-char start)
                 (if (= (preceding-char) ?$)
                     ;; Disregard already marked strings.
                     (setq start nil
                           end nil))))))
    (and start end
         (list (k-po-extract-unquoted (current-buffer) start end) start end))))

(defun k-po-mark-bash-string (start end keyword)
  "Mark the Bash string, from START to END, with '$'.  KEYWORD is ignored.
Leave point after marked string."
  (goto-char start)
  (insert "$")
  (goto-char (1+ end)))

;;; C or C++ mode specifics.

;;; A few long string cases (submitted by Ben Pfaff).

;; #define string "This is a long string " \
;; "that is continued across several lines " \
;; "in a macro in order to test \\ quoting\\" \
;; "\\ with goofy strings.\\"

;; char *x = "This is just an ordinary string "
;; "continued across several lines without needing "
;; "to use \\ characters at end-of-line.";

;; char *y = "Here is a string continued across \
;; several lines in the manner that was sanctioned \
;; in K&R C compilers and still works today, \
;; even though the method used above is more esthetic.";

;;; End of long string cases.

(defun k-po-find-c-string (keywords)
  "Find the next C string, excluding those marked by any of KEYWORDS.
Returns (CONTENTS START END) for the found string, or nil if none found."
  (let (start end)
    (while (and (not start)
                (re-search-forward "\\([\"']\\|/\\*\\|//\\)" nil t))
      (cond ((= (preceding-char) ?*)
             ;; Disregard comments.
             (search-forward "*/"))
            ((= (preceding-char) ?/)
             ;; Disregard C++ comments.
             (end-of-line)
             (forward-char 1))
            ((= (preceding-char) ?\')
             ;; Disregard character constants.
             (forward-char (if (= (following-char) ?\\) 3 2)))
            ((save-excursion
               (beginning-of-line)
               (looking-at "^# *\\(include\\|line\\)"))
             ;; Disregard lines being #include or #line directives.
             (end-of-line))
            ;; Else, find the end of the (possibly concatenated) string.
            (t (setq start (1- (point))
                     end nil)
               (while (not end)
                 (cond ((= (following-char) ?\")
                        (if (looking-at "\"[ \t\n\\\\]*\"")
                            (goto-char (match-end 0))
                          (forward-char 1)
                          (setq end (point))))
                       ((= (following-char) ?\\) (forward-char 2))
                       (t (skip-chars-forward "^\"\\\\"))))
               ;; Check before string for keyword and opening parenthesis.
               (goto-char start)
               (skip-chars-backward " \n\t")
               (if (= (preceding-char) ?\()
                   (progn
                     (backward-char 1)
                     (skip-chars-backward " \n\t")
                     (let ((end-keyword (point)))
                       (skip-chars-backward "_A-Za-z0-9")
                       (if (member (list (buffer-substring-no-properties (point)
                                                                         end-keyword))
                                   keywords)
                           ;; Disregard already marked strings.
                           (progn
                             (goto-char end)
                             (setq start nil
                                   end nil))
                         ;; String found.  Prepare to resume search.
                         (goto-char end))))
                 ;; String found.  Prepare to resume search.
                 (goto-char end)))))
    ;; Return the found string, if any.
    (and start end
         (list (k-po-extract-unquoted (current-buffer) start end) start end))))

(defun k-po-mark-c-string (start end keyword)
  "Mark the C string, from START to END, with KEYWORD.
Leave point after marked string."
  (goto-char end)
  (insert ")")
  (save-excursion
    (goto-char start)
    (insert keyword)
    (or (string-equal keyword "_") (insert " "))
    (insert "(")))

;;; Emacs LISP mode specifics.

(defun k-po-find-emacs-lisp-string (keywords)
  "Find the next Emacs LISP string, excluding those marked by any of KEYWORDS.
Returns (CONTENTS START END) for the found string, or nil if none found."
  (let (start end)
    (while (and (not start)
                (re-search-forward "[;\"?]" nil t))
      (cond ((= (preceding-char) ?\;)
             ;; Disregard comments.
             (search-forward "\n"))
            ((= (preceding-char) ?\?)
             ;; Disregard character constants.
             (forward-char (if (= (following-char) ?\\) 2 1)))
            ;; Else, find the end of the string.
            (t (setq start (1- (point)))
               (while (not (= (following-char) ?\"))
                 (skip-chars-forward "^\"\\\\")
                 (if (= (following-char) ?\\) (forward-char 2)))
               (forward-char 1)
               (setq end (point))
               ;; Check before string for keyword and opening parenthesis.
               (goto-char start)
               (skip-chars-backward " \n\t")
               (let ((end-keyword (point)))
                 (skip-chars-backward "-_A-Za-z0-9")
                 (if (and (= (preceding-char) ?\()
                          (member (list (buffer-substring-no-properties (point)
                                                                        end-keyword))
                                  keywords))
                     ;; Disregard already marked strings.
                     (progn
                       (goto-char end)
                       (setq start nil
                             end nil)))))))
    ;; Return the found string, if any.
    (and start end
         (list (k-po-extract-unquoted (current-buffer) start end) start end))))

(defun k-po-mark-emacs-lisp-string (start end keyword)
  "Mark the Emacs LISP string, from START to END, with KEYWORD.
Leave point after marked string."
  (goto-char end)
  (insert ")")
  (save-excursion
    (goto-char start)
    (insert "(" keyword)
    (or (string-equal keyword "_") (insert " "))))

;;; Python mode specifics.

(defun k-po-find-python-string (keywords)
  "Find the next Python string, excluding those marked by any of KEYWORDS.
Also disregard strings when preceded by an empty string of the other type.
Returns (CONTENTS START END) for the found string, or nil if none found."
  (let (contents start end)
    (while (and (not contents)
                (re-search-forward "[#\"']" nil t))
      (forward-char -1)
      (cond ((= (following-char) ?\#)
             ;; Disregard comments.
             (search-forward "\n"))
            ((looking-at "\"\"'")
             ;; Quintuple-quoted string
             (k-po-skip-over-python-string))
            ((looking-at "''\"")
             ;; Quadruple-quoted string
             (k-po-skip-over-python-string))
            (t
             ;; Simple-, double-, triple- or sextuple-quoted string.
             (if (memq (preceding-char) '(?r ?R))
                 (forward-char -1))
             (setq start (point)
                   contents (k-po-skip-over-python-string)
                   end (point))
             (goto-char start)
             (skip-chars-backward " \n\t")
             (cond ((= (preceding-char) ?\[)
                    ;; Disregard a string used as a dictionary index.
                    (setq contents nil))
                   ((= (preceding-char) ?\()
                    ;; Isolate the keyword which precedes string.
                    (backward-char 1)
                    (skip-chars-backward " \n\t")
                    (let ((end-keyword (point)))
                      (skip-chars-backward "_A-Za-z0-9")
                      (if (member (list (buffer-substring-no-properties (point)
                                                                        end-keyword))
                                  keywords)
                          ;; Disregard already marked strings.
                          (setq contents nil)))))
             (goto-char end))))
    ;; Return the found string, if any.
    (and contents (list contents start end))))

(defun k-po-skip-over-python-string ()
  "Skip over a Python string, possibly made up of many concatenated parts.
Leave point after string.  Return unquoted overall string contents."
  (let ((continue t)
        (contents "")
        raw start end resume)
    (while continue
      (skip-chars-forward " \t\n")      ; whitespace
      (cond ((= (following-char) ?#)    ; comment
             (setq start nil)
             (search-forward "\n"))
            ((looking-at "\\\n")        ; escaped newline
             (setq start nil)
             (forward-char 2))
            ((looking-at "[rR]?\"\"\"") ; sextuple-quoted string
             (setq raw (memq (following-char) '(?r ?R))
                   start (match-end 0))
             (goto-char start)
             (search-forward "\"\"\"")
             (setq resume (point)
                   end (- resume 3)))
            ((looking-at "[rr]?'''")    ; triple-quoted string
             (setq raw (memq (following-char) '(?r ?R))
                   start (match-end 0))
             (goto-char start)
             (search-forward "'''")
             (setq resume (point)
                   end (- resume 3)))
            ((looking-at "[rR]?\"")     ; double-quoted string
             (setq raw (memq (following-char) '(?r ?R))
                   start (match-end 0))
             (goto-char start)
             (while (not (memq (following-char) '(0 ?\")))
               (skip-chars-forward "^\"\\\\")
               (if (= (following-char) ?\\) (forward-char 2)))
             (if (eobp)
                 (setq contents nil
                       start nil)
               (setq end (point))
               (forward-char 1))
             (setq resume (point)))
            ((looking-at "[rR]?'")      ; single-quoted string
             (setq raw (memq (following-char) '(?r ?R))
                   start (match-end 0))
             (goto-char start)
             (while (not (memq (following-char) '(0 ?\')))
               (skip-chars-forward "^'\\\\")
               (if (= (following-char) ?\\) (forward-char 2)))
             (if (eobp)
                 (setq contents nil
                       start nil)
               (setq end (point))
               (forward-char 1))
             (setq resume (point)))
            (t                          ; no string anymore
             (setq start nil
                   continue nil)))
      (if start
          (setq contents (concat contents
                                 (if raw
                                     (buffer-substring start end)
                                   (k-po-extract-part-unquoted (current-buffer)
                                                             start end))))))
    (goto-char resume)
    contents))

(defun k-po-mark-python-string (start end keyword)
  "Mark the Python string, from START to END, with KEYWORD.
If KEYWORD is '.', prefix the string with an empty string of the other type.
Leave point after marked string."
  (cond ((string-equal keyword ".")
         (goto-char end)
         (save-excursion
           (goto-char start)
           (insert (cond ((= (following-char) ?\') "\"\"")
                         ((= (following-char) ?\") "''")
                         (t "??")))))
        (t (goto-char end)
           (insert ")")
           (save-excursion
             (goto-char start)
             (insert keyword "(")))))

;;; Miscellaneous features.

(defun k-po-statistics ()
  "Say how many entries in each category, and the current position."
  (interactive)
  (k-po-compute-counters t))

(defun k-po-validate ()
  "Use 'msgfmt' for validating the current PO file contents."
  (interactive)
  ;; The 'compile' subsystem is autoloaded through a call to (compile ...).
  ;; We need to initialize it outside of any binding. Without this statement,
  ;; all defcustoms and defvars of compile.el would be undone when the let*
  ;; terminates.
  (require 'compile)
  (let* ((dev-null
          (cond ((boundp 'null-device) null-device) ; since Emacs 20.3
                ((memq system-type '(windows-nt windows-95)) "NUL")
                (t "/dev/null")))
         (output
          (if k-po-keep-mo-file
              (concat (file-name-sans-extension buffer-file-name) ".mo")
            dev-null))
         (compilation-buffer-name-function
          (lambda (mode)
            (concat "*" mode " validation*")))
         (compile-command (concat k-po-msgfmt-program
                                  " --statistics -c -v -o "
                                  (shell-quote-argument output) " "
                                  (shell-quote-argument buffer-file-name))))
    (k-po-msgfmt-version-check)
    (compile compile-command)))

(defvar k-po-msgfmt-version-checked nil)
(defun k-po-msgfmt-version-check ()
  "'msgfmt' from GNU gettext 0.10.36 or greater is required."
  (with-temp-buffer
    (or
     ;; Don't bother checking again.
     k-po-msgfmt-version-checked

     (and
      ;; Make sure 'msgfmt' is available.
      (condition-case nil
          (call-process k-po-msgfmt-program
                        nil t nil "--verbose" "--version")
        (file-error nil))

      ;; Make sure there's a version number in the output:
      ;; 0.11 or 0.10.36 or 0.19.5.1 or 0.11-pre1 or 0.16.2-pre1
      (progn (goto-char (point-min))
             (or (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)[-_A-Za-z0-9]+$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)[-_A-Za-z0-9]+$")))

      ;; Make sure the version is recent enough.
      (>= (string-to-number
           (format "%d%03d%03d"
                   (string-to-number (match-string 1))
                   (string-to-number (match-string 2))
                   (string-to-number (or (match-string 3) "0"))))
          010036)

      ;; Remember the outcome.
      (setq k-po-msgfmt-version-checked t))

     (error "'msgfmt' from GNU gettext 0.10.36 or greater is required"))))

(defun k-po-guess-archive-name ()
  "Return the ideal file name for this PO file in the central archives."
  (let ((filename (file-name-nondirectory buffer-file-name))
        start-of-header end-of-header package version team)
    (save-excursion
      ;; Find the PO file header entry.
      (goto-char (point-min))
      (re-search-forward k-po-any-msgstr-block-regexp)
      (setq start-of-header (match-beginning 0)
            end-of-header (match-end 0))
      ;; Get the package and version.
      (goto-char start-of-header)
      (if (re-search-forward "\n\
\"Project-Id-Version: \\(GNU \\|Free \\)?\\([^\n ]+\\) \\([^\n ]+\\)\\\\n\"$"
                             end-of-header t)
          (setq package (match-string-no-properties 2)
                version (match-string-no-properties 3)))
      (if (or (not package) (string-equal package "PACKAGE")
              (not version) (string-equal version "VERSION"))
          (error "Project-Id-Version field does not have a proper value"))
      ;; File name version and Project-Id-Version must match
      (cond (;; A `filename' w/o package and version info at all
             (string-match "^[^\\.]*\\.po\\'" filename))
            (;; TP Robot compatible `filename': PACKAGE-VERSION.LL.po
             (string-match (concat (regexp-quote package)
                                   "-\\(.*\\)\\.[^\\.]*\\.po\\'") filename)
             (if (not (equal version (match-string-no-properties 1 filename)))
                 (error "\
Version mismatch: file name: %s; header: %s.\n\
Adjust Project-Id-Version field to match file name and try again"
                        (match-string-no-properties 1 filename) version))))
      ;; Get the team.
      (if (stringp k-po-team-name-to-code)
          (setq team k-po-team-name-to-code)
        (goto-char start-of-header)
        (if (re-search-forward "\n\
\"Language-Team: \\([^ ].*[^ ]\\) <.+@.+>\\\\n\"$"
                               end-of-header t)
            (let ((name (match-string-no-properties 1)))
              (if name
                  (let ((pair (assoc name k-po-team-name-to-code)))
                    (if pair
                        (setq team (cdr pair))
                      (setq team (read-string (format "\
Team name '%s' unknown.  What is the team code? "
                                                      name)))))))))
      (if (or (not team) (string-equal team "LL"))
          (error "Language-Team field does not have a proper value"))
      ;; Compose the name.
      (concat package "-" version "." team ".po"))))

(defun k-po-guess-team-address ()
  "Return the team address related to this PO file."
  (let (team)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward k-po-any-msgstr-block-regexp)
      (goto-char (match-beginning 0))
      (if (re-search-forward
           "\n\"Language-Team: +\\(.*<\\(.*\\)@.*>\\)\\\\n\"$"
           (match-end 0) t)
          (setq team (match-string-no-properties 2)))
      (if (or (not team) (string-equal team "LL"))
          (error "Language-Team field does not have a proper value"))
      (match-string-no-properties 1))))

(defun k-po-send-mail ()
  "Start composing a letter, possibly including the current PO file."
  (interactive)
  (let* ((team-flag (y-or-n-p
                     "\
Write to your team (y) or the Translation Project bot (n)? "))
         (address (if team-flag
                      (k-po-guess-team-address)
                    k-po-translation-project-address)))
    (if (not (y-or-n-p "Include current PO file in mail? "))
        (apply k-po-compose-mail-function address
               (read-string "Subject? ") nil)
      (when (buffer-modified-p)
        (error "The file has not been saved, and it has not been validated"))
      (when (and (y-or-n-p "Have you validated ('V') this file? ")
                 (or (zerop k-po-untranslated-counter)
                     (y-or-n-p
                      (format "%d entries are untranslated, include anyway? "
                              k-po-untranslated-counter)))
                 (or (zerop k-po-fuzzy-counter)
                     (y-or-n-p
                      (format "%d entries are still fuzzy, include anyway? "
                              k-po-fuzzy-counter)))
                 (or (zerop k-po-obsolete-counter)
                     (y-or-n-p
                      (format "%d entries are obsolete, include anyway? "
                              k-po-obsolete-counter))))
        (let ((buffer (current-buffer))
              (name (k-po-guess-archive-name))
              (transient-mark-mode nil)
              (coding-system-for-read buffer-file-coding-system)
              (coding-system-for-write buffer-file-coding-system))
          (apply k-po-compose-mail-function address
                 (if team-flag
                     (read-string "Subject: ")
                   (format "%s %s" k-po-translation-project-mail-label name))
                 nil)
          (goto-char (point-min))
          (re-search-forward
           (concat "^" (regexp-quote mail-header-separator) "\n"))
          (save-excursion
            (save-restriction
              (narrow-to-region (point) (point))
              (insert-buffer-substring buffer)
              (shell-command-on-region
               (point-min) (point-max)
               (concat k-po-gzip-uuencode-command " " name ".gz") t t)))))))
  (message ""))

(defun k-po-confirm-and-quit ()
  "Confirm if quit should be attempted and then, do it.
This is a failsafe.  Confirmation is asked if only the real quit would not."
  (interactive)
  (when (k-po-check-all-pending-edits)
    (when (or (buffer-modified-p)
              (> k-po-untranslated-counter 0)
              (> k-po-fuzzy-counter 0)
              (> k-po-obsolete-counter 0)
              (y-or-n-p "Really quit editing this PO file? "))
      (k-po-quit))
    (message "")))

(defun k-po-quit ()
  "Save the PO file and kill buffer.
However, offer validation if appropriate and ask confirmation if untranslated
strings remain."
  (interactive)
  (when (k-po-check-all-pending-edits)
    (let ((quit t))
      ;; Offer validation of newly modified entries.
      (when (and (buffer-modified-p)
                 (not (y-or-n-p
                       "File was modified; skip validation step? ")))
        (message "")
        (k-po-validate)
        ;; If we knew that the validation was all successful, we should
        ;; just quit.  But since we do not know yet, as the validation
        ;; might be asynchronous with PO mode commands, the safest is to
        ;; stay within PO mode, even if this implies that another
        ;; 'k-po-quit' command will be later required to exit for true.
        (setq quit nil))
      ;; Offer to work on untranslated entries.
      (when (and quit
                 (or (> k-po-untranslated-counter 0)
                     (> k-po-fuzzy-counter 0)
                     (> k-po-obsolete-counter 0))
                 (not (y-or-n-p
                       "Unprocessed entries remain; quit anyway? ")))
        (setq quit nil)
        (k-po-auto-select-entry))
      ;; Clear message area.
      (message "")
      ;; Or else, kill buffers and quit for true.
      (when quit
        (save-buffer)
        (kill-buffer (current-buffer))))))

(defun k-po-compile (po mo)
  "Compile PO file to MO (a path)."
  (interactive
   (let* ((po (if (derived-mode-p 'k-po-mode)
                  buffer-file-name
                (read-file-name "Source PO file: ")))
          (mo (if current-prefix-arg
                  (f-join (read-directory-name "Place compiled MO at: ")
                          (concat (f-base po) ".mo"))
                (f-join (xdg-data-home)
                        "locale"
                        "zh_TW"
                        "LC_MESSAGES"
                        (concat (f-base po) ".mo")))))
     (list po mo)))
  (make-directory (f-dirname mo) t)
  (let (status output)
    (with-temp-buffer
      (setq status (call-process "msgfmt" nil t nil
                                 "-o" mo po))
      (setq output (string-trim (buffer-string))))
    (if (= 0 status)
        (message "Compiled %s to %s" po mo)
      (message "Something went wrong. See *k/po* for output")
      (with-current-buffer (get-buffer-create "*k/po*")
        (erase-buffer)
        (insert (format "%s" output))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.po[tx]?\\'\\|\\.po\\." . k-po-mode))

(provide 'k-po-mode)

;;; k-po-mode.el ends here
