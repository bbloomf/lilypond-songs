\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Battle Hymn of the Republic"}}
  poet = \markup\oldStyleNum"Julia Ward Howe (1819–1910)"
  composer = \markup\oldStyleNum"William Steffe (1830–1890)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 70))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #40
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  f16[\mf f] |
  f8. f16 f8. ees16 d8. f16 bes8. c16 |
  d8. d16 d8. c16 bes4 bes8. a16 |
  
  g8. g16 g8. a16 bes8. a16 bes8. g16 |
  f8. g16 f8. d16 f4 f8. f16 |
  
  f8. f16 f8. ees16 d8. f16 bes8. c16 |
  d8. d16 d8. c16 bes4 bes |
  c c bes a |
  bes1 |
  
  f4.\f ees8 d8. f16 bes8. c16 |
  d2 bes |
  g4. a8 bes8. a16 bes8. g16 |
  f2 d |
  
  f4. ees8 d8. f16 bes8. c16 |
  d2 bes4 bes |
  c c bes a |
  bes2. b8\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Mine eyes have seen the glo -- ry of the com -- ing of the Lord;
  He is tramp -- ling out the vin -- tage where the grapes of wrath are stored;
  He hath loosed the fate -- ful light -- ning of His ter -- ri -- ble swift sword;
  His truth is march -- ing on.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  I have seen Him in the watch -- fires of a hun -- dred cir -- cling camps;
  They have build -- ed Him an al -- tar in the eve -- ning dews and damps;
  I can read His right -- eous sen -- tence in the dim and flar -- ing lamps:
  His day is march -- ing on.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  I have read a fie -- ry gos -- pel writ in bur -- nished rows of steel:
  “As ye deal with My con -- tem -- ners, so with you My grace shall deal:”
  Let the He -- ro born of wo -- man crush the ser -- pant with His heel,
  Since God is march -- ing on.
  
  Glo -- ry, glo -- ry, Hal -- le -- lu -- jah!
  Glo -- ry, glo -- ry, Hal -- le -- lu -- jah!
  Glo -- ry, glo -- ry, Hal -- le -- lu -- jah!
  His truth is march -- ing on.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  He has sound -- ed forth the trum -- pet that shall nev -- er call re -- treat;
  He is sift -- ing out the hearts of men be -- fore His judg -- ment seat:
  Oh, be swift, my soul, to an -- swer Him! be ju -- bi -- lant, my feet!
  Our God is march -- ing on.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  In the beau -- ty of the lil -- ies Christ was born a -- cross the sea,
  With a glo -- ry in His bo -- som that trans -- fig -- ures you and me;
  As He died to make men ho -- ly, let us die to make men free,
  While God is march -- ing on.
}

altoMusic = \relative c' {
  \partial 8
  d16[ d] |
  d8. d16 d8. c16 bes8. d16 f8. f16 |
  f8. f16 f8. f16 d4 \bar"" f8. f16 |
  
  ees8. ees16 ees8. ees16 ees8. ees16 ees8. ees16 |
  d8. ees16 d8. bes16 d4 \bar"" d8. d16 |
  
  d8. d16 d8. c16 bes8. d16 f8. f16 |
  f8. f16 f8. ees16 d4 d |
  g g f f |
  f1 |
  
  d4. c8 bes8. d16 f8. f16 |
  f2 d |
  ees4. ees8 ees8. ees16 ees8. ees16 |
  d2 bes |
  
  c4. c8 bes8. c16 g'8. g16 |
  fis2 d4 d |
  g g f f |
  f2. s8 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \partial 8
  bes16[ bes] |
  bes8. bes16 bes8. bes16 f8. bes16 bes8. a16 |
  bes8. bes16 bes8. a16 bes4 bes8. bes16 |
  
  bes8. bes16 bes8. c16 bes8. c16 bes8. bes16 |
  bes8. bes16 bes8. f16 bes4 bes8. bes16 |
  
  bes8. bes16 bes8. bes16 f8. bes16 bes8. a16 |
  bes8. bes16 bes8. a16 bes4 bes |
  ees ees d c |
  d1 |
  
  bes4 bes4 f8. bes16 bes8. a16 |
  bes2 bes |
  bes4. c8 bes8. c16 bes8. a16 |
  bes2 bes |
  
  a4. a8 f8. a16 d8. c16 |
  a2 bes4 bes |
  ees ees d c |
  d2. s8 \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,16[ bes] |
  bes8. bes16 bes8. bes16 bes8. bes16 d8. f16 |
  bes8. bes16 bes8. f16 bes,4 d8. d16 |
  
  ees8. ees16 ees8. f16 g8. f16 g8. ees16 |
  bes8. bes16 bes8. bes16 bes4 bes8. bes16 |
  
  bes8. bes16 bes8. bes16 bes8. f'16 d8. c16 |
  bes8. d16 f8. fis16 g4 f |
  ees ees f f |
  bes,1 |
  
  bes4 bes bes8. f'16 d8. c16 |
  bes2 bes4( d) |
  ees4. f8 g8. f16 g8. a16 |
  bes4( a) g( f) |
  
  f4. f8 bes,8. f'16 g8. ees16 |
  d2 g4 f |
  ees ees f f |
  bes,2. d8\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 100
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
}


