\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Life on the Ocean Wave"}}
  composer = \markup\oldStyleNum"Henry Russell (1813–1900)"
  poet = \markup\oldStyleNum"Epes Sargent (1813–1880)"
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
       (padding . -3)
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  \set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  d8 |
  d g g g[ g] a |
  b4.~ b8 b\rest g16[ a] |
  b8[ b] b c4 b8 |
  a4.~ a8 b\rest \bar"" a16[ fis] |
  
  d8[ d] fis a[ a] b8 |
  c4.~ c8 b\rest c16[ d] |
  e8[ e] d c8[ b] a |
  g4.~ g8 b\rest \bar"||"\break
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark \markup\smallCapsOldStyle"Fine"
  
  g16[ fis] |
  e4\mp fis16[ fis] g4 a8 |
  b4.~ b8 b\rest g16 fis |
  e4 fis16[ fis] g4 a8 |
  b4.~ b8 b\rest b16[ b] |
  
  b8[ b] b b4 b8 |
  b4.~ b8 b\rest b |
  b[ b] b b[ b] b |
  c4.( d,4)\fermata \bar"||"
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark \markup\italic"Sing first verse in D.C."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set ignoreMelismata = ##t
  A life on the o -- _ cean wave, _
  A __ _ home on the roll -- ing deep, __ _
  Where the scat -- _ tered wa -- _ ters rave, __ _
  And the winds _ their rev -- _ els keep: __ _
  
  Like an ea -- gle __ _ caged I pine __ _
  On this dull, un -- _ chang -- ing shore; _
  Oh! __ _ give me the flash -- ing brine, _
  The spray and the tem -- _ pest roar! _
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
	\set ignoreMelismata = ##t
  Once more on the deck _ I stand _
  Of my own __ _ swift -- glid -- ing craft, __ _ 
  Set __ _ sail! _ fare -- well to the land, __ _
  The __ _ gale fol -- lows far __ _ a -- baft. __ _
  
  We __ _ shoot thro’ the spark -- ling foam, __ _
  Like an o -- cean _ bird set free; __ _
  Like the o -- _ cean bird, our home _
  We’ll find __ _ far out on the sea! __ _
}

sopWordsIII = \lyricmode {
	\set ignoreMelismata = ##t
  \set stanza = #"3. "
  The land is no long -- er in view, _
  The _ clouds have be -- gun to frown, _
  But __ _ with a stout ves -- sel and crew __ _
  We’ll _ say, “Let the storm _ come down!” _
  
  And the song of our heart shall be, __ _
  While the winds and the wa -- ters rave, __ _
  A __ _ life on the heav -- ing sea, __ _
  A home on the bound -- _ ding wave! _
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d8 |
  d d d d[ d] fis |
  g4.~ g8 s g16[ fis] |
  g8[ g] g a4 g8 |
  fis4.~ fis8 s d16[ d] |
  
  d8[ d] d fis[ fis] g8 |
  a4.~ a8 s fis16[ fis] |
  fis8[ fis] fis fis8[ g] fis |
  g4.~ g8 s 
  
  d16[ d] |
  e4 dis16[ dis] e4 e8 |
  dis4.~ dis8 s dis16 dis |
  e4 dis16[ dis] e4 e8 |
  dis4.~ dis8 s dis16[ dis] |
  
  dis8[ dis] dis dis4 dis8 |
  e4.~ e8 s e |
  eis8[ eis] eis eis[ eis] eis |
  fis4.( d4) \bar"|."
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
  d,8 |
  b' b b b[ b] d |
  d4.~ d8 s b16[ c] |
  d8[ d] d d4 d8 |
  d4.~ d8 s fis,16[ fis] |
  
  fis8[ fis] a d[ d] d8 |
  d4.~ d8 s a16[ b] |
  c8[ c] b a8[ b] c |
  b4.~ b8 s 
  
  g16[ g] |
  g4 g16[ g] g4 e8 |
  fis4.~ fis8 s a16 a |
  g4 g16[ g] g4 e8 |
  fis4.~ fis8 s fis16[ fis] |
  
  fis8[ fis] fis fis4 fis8 |
  g4.~ g8 s g |
  gis[ gis] gis gis[ gis] gis |
  a4.( d,4) \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,8 |
  g g g g[ g] d |
  g4.~ g8 d\rest g16[ g] |
  g8[ g] g fis4 g8 |
  d4.~ d8 d\rest d16[ d] |
  
  d8[ d] d d[ d] d8 |
  d4.~ d8 d\rest d16[ d] |
  d8[ d] d d4 d8 |
  g4.~ g8 d\rest 
  
  b16[ b] |
  c4 c16[ c] c4 c8 |
  b4.~ b8 d\rest b16 b |
  c4 c16[ c] c4 c8 |
  b4.~ b8 d\rest b16[ b] |
  
  b8[ b] b b4 b8 |
  e4.~ e8 d\rest e |
  d[ d] d d[ d] d |
  d4.~ d4\fermata \bar"|."
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
    \tempo 4 = 140
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


