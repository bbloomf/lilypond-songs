\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The harp that once through Tara’s halls"}}
  poet = \markup\oldStyleNum"Thomas Moore (1779–1852)"
  composer = \markup\oldStyleNum{"Irish Air," \italic"Gramachree"}
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
       (padding . 0.2)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 0))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 80))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #36
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 ees4 |
  bes'4. c8 bes4 g |
  c4. d8 ees4 c 
  bes4. g8 f4. g8 
  ees2 b'4\rest bes |
  
  ees4. d8 ees4 f |
  ees d c bes |
  c bes ees g, |
  bes2 b4\rest bes4 |
  ees4. d8 ees4 g8[ f] |
  ees4 d c bes |
  
  c bes aes g |
  c2 b4\rest d4 |
  ees4. d8 c4 bes |
  c4 d ees\fermata c |
  bes g f4. g8 |
  ees2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The harp that once through Ta -- ra’s halls,
  The soul of mu -- sic shed,
  Now hangs as mute on Ta -- ra’s walls,
  As if that soul were fled;
  So sleeps the pride of form -- er days,
  So glo -- ry’s thrill is o’er;
  And hearts that once beat high for praise,
  Now feel that pulse no more.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  No more to chiefs and la -- dies bright,
  The harp of Ta -- ra swells;
  The chord, a -- lone, that breaks at night,
  Its tale of ru -- in tells:
  Thus Free -- dom now so sel -- dom wakes,
  The on -- ly throb she gives
  Is when some heart in -- dig -- nant breaks,
  To show that still she lives.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees4 |
  ees4. ees8 ees4 ees |
  ees4. ees8 ees4 ees |
  ees4. ees8 d4. d8 |
  ees2 s4 g |
  g4. f8 g4 f |
  
  f f aes f |
  g g g ees |
  f2 s4 f |
  g4. f8 g4 aes |
  g f aes f |
  
  ees ees ees ees |
  ees2 s4 aes |
  g4. bes8 aes4 g |
  ees aes g fis |
  g ees d4. d8 |
  bes2. \bar"|."
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
  g4 |
  g4. aes8 g4 bes |
  aes4. bes8 c4 aes |
  g4. bes8 aes4. bes8 |
  g2 s4 g4 |
  c4. c8 c4 c |
  
  bes bes ees bes |
  bes bes c c |
  bes2 s4 bes |
  bes4. bes8 bes4 c |
  bes bes ees bes |
  
  aes g c bes |
  aes2 s4 bes |
  bes4. ees8 ees4 ees |
  c c c ees |
  ees bes aes4. bes8 |
  g2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 |
  ees4. ees8 ees4 ees |
  aes,4. aes8 aes4 aes |
  bes4. bes8 bes4. bes8 |
  ees2 d4\rest ees |
  c4. c8 c4 a |
  
  bes bes c d |
  ees ees c c |
  d2 d4\rest d |
  ees4. ees8 ees4 aes, |
  bes bes c d |
  
  ees ees ees ees |
  aes2 d,4\rest f |
  ees4. g8 aes4 ees |
  aes f c\fermata a |
  bes bes bes4. bes8 |
  ees2. \bar"|."
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
    \new Lyrics = "altos"
    \new Lyrics = "altosII"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos"  \lyricsto "tenors" \sopWords
    \context Lyrics = "altosII"  \lyricsto "tenors" \sopWordsII
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


