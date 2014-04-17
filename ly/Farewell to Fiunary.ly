\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Farewell to Fiunary"}}
  composer = \markup\oldStyleNum"Norman MacLeod (1812–1872)"
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
       (padding . -5)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
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
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8 g'8 |
  g4 g8 d'4 b8 |
  a4 g8 fis4 a8 |
  
  b4 e,8 e4 fis8 |
  e4 d8 d4 d8 |
  e4 e8 g4 g8 |
  a4 b8 d4 d8 |

  e4 e8 d4 b8 |
  a4 g8 g4. \bar"||"\break
  g4 g8 d'4 b8 |

  a4 g8 fis4. |
  b4 e,8 e4 fis8 |
  e4 d8 d4. |
  e4 e8 g4 g8 |

  a4 b8 d4 d8 |
  e4 e8 d4 b8 |
  a4 g8 g4. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The wind is fair, the day is fine,
  And swift -- ly, swift -- ly runs the time;
  The boat is float -- ing on the tide
  That wafts me off from Fiu -- na -- ry.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  A thou -- sand, thou -- sand ten -- der ties
  A -- wake this day my plain -- tive sighs,
  My heart with -- in me al -- most dies
  At thought of leav -- ing Fiu -- na -- ry.

  We must up and haste a -- way,
  We must up and haste a -- way,
  We must up and haste a -- way,
  Fare -- well, fare -- well to Fiu -- na -- ry.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  But I must leave those hap -- py vales, ""
  See, they spread the flap -- ping sails!
  A -- dieu, a -- dieu my na -- tive dales!
  Fare -- well, fare -- well to Fiu -- na -- ry.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8 |
  d4 d8 g4 d8 |
  e4 e8 d4 fis8 |
  g4 e8 e4 d8 |
  c4 d8 d4 d8 |

  b4 b8 e4 e8 |
  e4 e8 fis4 fis8 |
  g4 g8 g4 g8 |
  e[ c] e d4. \bar"||"

  b8[ d] g fis4 fis8 |
  e[ c] e d4.
  g8[ e] e c4 c8 |
  b[ g] d' d4. |

  e4 e8 e4 e8 |
  e[ c] e fis4 fis8 |
  g[ e] g g4 g8 |
  e[ c] c b4.
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
  g8 |
  b4 b8 g4 g8 |
  c4 c8 a4 a8 |
  b4 g8 g4 a8 |
  g4 fis8 fis4 fis8 |

  g4 g8 g4 g8 |
  c4 b8 b4 b8 |
  c[ e] c b[ g] b |
  c4 c8 b4. \bar"||"

  g4 b8 d[ b] d |
  c4 c8 a[ fis a] |
  b4 g8 g[ a] a |
  g4 fis8 fis[ d fis] |

  g[ b] g g[ e] g |
  c4 b8 b[ d] b |
  c4 c8 b[ d] d |
  c4 g8 g4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8 |
  g,4 g8 b4 b8 |
  c4 c8 d4 d8 |

  e4 e8 e4 d8 |
  c4 d8 d4 d8 |
  e4 e8 c4 c8 |
  a4 g8 b4 b8 |
  
  c4 c8 g4 g8 |
  c4 e8 g4. \bar"||"
  g,4 g8 b4 b8 |

  c4 c8 d4. |
  e4 e8 c4 d8 |
  e4 d8 d4. |
  e4 e8 c4 c8 |

  a4 g8 b4 b8 |
  c4 c8 g4 b8 |
  c4 d8 g,4. \bar"|."
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
    \tempo 4 = 105
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
