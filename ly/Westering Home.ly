\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Westering Home"}}
  composer = \markup\oldStyleNum"Hugh S. Roberton"
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
       (padding . 3)
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
  \key a \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  e8.^\markup\italic"Chorus" fis16 e8 e cis e |
  a a b cis4. |
  d8. cis16 b8 cis8. b16 a8 |
  fis fis e e4. |

  e8. fis16 e8 e cis e |
  a a b cis4. |
  e8. cis16 a8 cis4 b8 |
  a4. a \bar"||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark\markup\italic"Fine"

  e'8 cis a a b cis |
  b8. a16 b8 e,4. |
  e8 a a a b cis |
  d8. cis16 d8 b4. |
  e8 cis a a b cis |
  b8. a16 b8 e,4 \bar"" d'8 |

  cis8. b16 a8 cis cis b |
  a4. a4. \bar"||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark"D.C."
}
sopWords = \lyricmode {
  West -- er -- ing home, and a song in the air,
  Light in the eye and it’s good -- bye to care.
  Laugh -- ter o’ love, and a wel -- com -- ing there,
  Isle of my heart, my own one.
  \set stanza = #"1. "
  Tell me o’ lands o’ the O -- ri -- ent gay;
  Speak o’ the rich -- es and joys o’ Ca -- thay.
  Eh, but it’s grand to be wak -- in’ ilk day
  To find your -- self near -- er to Is -- la.
}

sopWordsII = \lyricmode {
  \repeat unfold 37 ""
  \set stanza = #"2. "
  Where are the folk like the folk o’ the west?
  Can -- ty and cou -- thy and kind -- ly, the best;
  There I would hie me and there I would rest
  At hame wi’ my ain folk in Is -- la.
}

sopWordsIII = \lyricmode {
  %{\repeat unfold 37 ""
  \set stanza = #"3. "
  Now I’m at home and at home I do lay
  Dream -- ing of rich -- es that come from Ca -- thay;
  I’ll hop a good ship and be on my way,
  And bring back my for -- tune to Is -- la.%}
}

sopWordsIV = \lyricmode {
}

sopWordsV = \lyricmode {
}

altoMusic = \relative c' {
  cis8. cis16 cis8 cis a cis |
  e8 e gis a4. |
  fis8. fis16 gis8 a8. gis16 e8 |
  d d d cis4. |

  cis8. cis16 cis8 cis a cis |
  e8 e gis a4. |
  a8. e16 fis8 e4 d8 |
  cis4. cis \bar"||"


  cis8 e cis cis fis fis |
  fis8. fis16 fis8 d4. |
  cis8 cis cis cis fis fis |
  fis8. fis16 fis8 gis4. |

  cis,8 e cis cis fis fis |
  fis8. fis16 fis8 d4 fis8 |
  e8. d16 fis8 e e d |
  cis4. cis \bar"||"
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
  e,8. a16 e8 e e e |
  a a e e4. |
  a8. a16 b8 e,8. e16 a8 |
  a a a a4. |

  e8. a16 e8 e e e |
  a a e e4. |
  a8. a16 cis8 a4 gis8 |
  a4. a \bar"||"


  a8 a e fis a a |
  d8. d16 d8 b4. |
  a8 e e fis a a |
  b8. a16 b8 d4. |

  a8 a e fis a a |
  d8. d16 d8 b4 a8 |
  a8. gis16 cis8 a a gis |
  a4. a \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a,8. a16 a8 a a a |
  cis cis e a,4. |
  d8. fis16 e8 a,8. a16 cis8 |
  d d d a4. |

  a8. a16 a8 a a a |
  cis cis e a,4. |
  cis8. cis16 fis8 e4 e8 
  a,4. a \bar"||"

  a8 a a fis fis fis |
  b8. d16 b8 a4. |
  a8 a a fis fis fis |
  b8. cis16 b8 e4. |

  a,8 a a fis fis fis |
  b8. d16 b8 a4 d8 |
  a8. e'16 fis8 e e e 
  a,4. a \bar"||"
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
         (padding . -1)
         (stretchability . 2))
    }
  }
}
