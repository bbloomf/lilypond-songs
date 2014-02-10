\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #58
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
	\partial 4
  c8[ e] |
  f4 f8 g a4 g8. f16 |
  g4 g8 f d4 c8 e |
  f4 f8~ f f4 a8 c |
  d2 c4 c8~ c |
  
  \slurDashed d~ d d8( c) a4~ a8 c |
  bes a g f d4\fermata c8 e |
  f16~ f8. a16( c8.) d4 c8 a |
  g2 f4 c\fermata | \break
  
  f4 f8 g a4 a8 g |
  f4 f8 d c4 c8 e |
  f4 f8 f f4 a8 c |
  d2 c4 c |
  
  d d8 c a4 a8 c |
  bes a g f d4\fermata c8 e |
  f16 f8. a16 c8. d4 c8 a |
  \tieSolid
  g2 f~ |
  f4 \bar"|."
}
sopWords = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"1. "
	By _ yon bon -- nie banks,
  And by yon bon -- nie braes,
  Where the sun shines _ bright on Loch Lo -- mond,
  Where _ me __ _ and my true love
  Were ev -- er wont to gae
  On the bon -- nie, bon -- nie banks of Loch Lo -- mond.
  Oh!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  ’Twas _ there that we part -- ed
  In yon shad -- y glen
  On the steep, steep _ side of Ben Lo -- mond
  Where in pur -- _ ple __ _ hue __ _
  The High -- land hills we view
  And the moon _ com -- in’ out in the gloam -- ing.
  
  Oh! ye’ll take the high road and I’ll take the low road,
  And I’ll be in Scot -- land a -- fore ye,
  But me and my true love we’ll nev -- er meet a -- gain
  On the bon -- nie, bon -- nie banks of Loch Lo -- mond. _
}

sopWordsIII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"3. "
  The _ wee bird -- ies sang
  And the wild flow -- ers spring
  And in sun -- shine the wa -- ters are sleep -- ing,
  But the brok -- en heart it kens __ _
  Nae sec -- ond Spring a -- gain
  Tho’ the wae -- ful may _ cease frae their greet -- ing. Oh!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c4 |
  c c8 e f4 e8. f16 |
  d4 d8 d bes4 c8 c |
  c4 e8~ e d4 ees8 ees |
  \slurSolid d4( f) e e8~ e |
  
  f~ f f~ f e4~ e8 e |
  d d c c bes4 bes8 e |
  d16~ d8. e16~ e8. f4 f8 f |
  e2 f4 c |
  
  c d8 d e4 cis8 cis |
  d4 d8 bes bes4 c8 c |
  c4 d8 d d4 f8 f |
  \slurSolid f4( e) f4 f |
  
  f f8 f e4 e8 e |
  d d c c bes4 c8 c |
  c16 c8. f16 f8. f4 f8 f |
  \tieSolid
  e2 f2~ |
  f4 \bar"|."
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
  a4 |
  a a8 bes c4 bes8. a16 |
  bes4 bes8 bes f4 e8 g |
  a4 a8~ a a4 f8 f |
  \slurSolid f4( g) g a8~ a |
  
  a~ a a~ a a4~ a8 a |
  g g f f f4 e8 bes' |
  a16~ a8. a16~ a8. bes4 f8 f |
  bes2 a4 bes |
  
  a4 a8 a a4 a8 a |
  f4 g8 g e4 g8 g |
  f4 a8 a a4 c8 a |
  bes2 c4 a |
  
  a4 a8 a a4 a8 a |
  g g f f f4 g8 g |
  f16 a8. a16 a8. bes4 f8 c' |
  \tieSolid
  bes2 a~ |
  a4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 |
  f f8 f f4 f8. f16 |
  bes,4 bes8 bes bes4 c8 c |
  f4 f8~ f d4 c8 c |
  \slurSolid bes4( g) c a8~ a |
  
  d~ d d~ d c4~ c8 c |
  g g a a bes4\fermata c8 c |
  d16~ d8. c16~ c8. bes4 a8 a |
  c2 f4 e\fermata |
  
  f d8 d cis4 a8 a |
  bes4 g8 g c4 c8 c |
  a4 d8 d d4 f8 f |
  \slurSolid bes,4( g) a f' |
  
  d d8 d c4 c8 c |
  g g a a bes4\fermata bes8 bes |
  a16 f8. f'16 f8. bes,4 a8 f |
  \tieSolid
  c'2 f~ |
  f4 \bar"|."
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
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Loch Lomond"}}
  composer = \markup\oldStyleNum"Scottish Folk Song"
  tagline = ""
}}


