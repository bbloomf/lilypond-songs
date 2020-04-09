\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Shenendoah"}}
  composer = \markup\oldStyleNum"Folk Song"
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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##t
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
  \key d \major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 2 a2 |
  d4 d d2~ |
  d4 e fis g |
  b a2.~ |
  a2 d4( cis) |

  b1~ |
  b4 a b a |
  fis a2.~ |
  a2 a |
  b4 b b2~ |
  
  b4 fis a fis |
  e4 d2.~ |
  d2 e |
  fis1~ |
  fis4 d fis b |
  
  a1~ |
  a2 d,4 e |
  fis2. d4 |
  e2 d~ |
  d2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Oh Shen -- an -- doah, I long to hear you __
  A -- way you roll -- ing riv -- er __
  Oh Shen -- an -- doah, I long to hear you __
  A -- way, I’m bound a -- way
  ’Cross the wide Mis -- sou -- ri. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oh Shen -- an -- doah, I love your daugh -- ter __
  A -- way you roll -- ing riv -- er __
  I’ll take her ’cross that roll -- ing wa -- ter __
  A -- way, I’m bound a -- way
  ’Cross the wide Mis -- sou -- ri. __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  This white man loves your In -- dian maid -- en __
  A -- way you roll -- ing riv -- er __
  In my ca -- noe with no -- tions la -- den __
  A -- way, I’m bound a -- way
  ’Cross the wide Mis -- sou -- ri. __
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Fare -- well, good -- bye, __ I shall not grieve you __
  A -- way you roll -- ing riv -- er __
  Oh Shen -- an -- doah, I’ll not de -- ceive you __
  A -- way, we’re bound a -- way
  ’Cross the wide Mis -- sou -- ri. __
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  a2 |
  d4 d d2~ |
  d4 cis d e |
  g fis2.~ |
  fis2 b4( a) |
  
  g1~ |
  g4 fis g fis |
  d4 fis2.~ |
  fis2 fis |
  fis4 fis fis2~ |

  fis4 cis fis cis |
  b4 b2.~ |
  b2 b |
  d1~ |
  d4 d d d |

  cis1~ |
  cis2 b4 cis |
  d2. d4 |
  cis2 d~ |
  d \bar"|."
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
  a2 |
  fis4 fis fis2~ |
  fis4 a a cis |
  d4 d2.~ |
  d2 d4( e) |

  d1~ |
  d4 d d d |
  d a2.~ |
  a2 a |
  d4 d d2~ |

  d4 a cis a |
  g g2.~ |
  g2 g |
  a1~ |
  a4 fis a b |

  e,1~ |
  e2 g4 a |
  a2. fis4 |
  g2 fis~ |
  fis2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a2 |
  d,4 d d2~ |
  d4 a a a |
  g d'2.~ |
  d2 d2 |

  g,1~ |
  g4 d' d d |
  d d2.~ |
  d2 d |
  b4 b b2~ |
  
  b4 fis4 fis fis |
  g4 g2.~ |
  g2 g2 |
  d'1~ |
  d4 d d b |

  a1~ |
  a2 g4 g |
  d'2. d4 |
  a2 d~ |
  d \bar"|."
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
    \tempo 4 = 90
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
         (padding . 1)
         (stretchability . 2))
    }
  }
}
