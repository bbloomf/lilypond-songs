\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Gaily the Troubadour"}}
  composer = \markup\oldStyleNum"Thomas Haynes Bayly (1797–1839)"
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
       (padding . 4)
       (stretchability . 150))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #60
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
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  g'4 bes8 g |
  ees ees ees4 |

  f4 ees8 f |
  g4( ees8) bes'\rest |
  g4 bes8 g |
  ees ees ees4 |
  f g8 f |

  ees4 bes'\rest |
  \repeat unfold 2 {
    ees d8 c |
    bes g ees4 |
    c' bes8 aes |
    g4 bes\rest |

    ees,8 f g4 |
    aes8 bes c4 |
    c8 bes4 d,8 |
    ees4 bes'\rest |
  }
  \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Gai -- ly the Trou -- ba -- dour touch’d his gui -- tar,
  When he was has -- ten -- ing home from the war:
  \repeat unfold 2 {
    Sing -- ing, “From Pal -- es -- tine hith -- er I come,
    La -- dy love! la -- dy love! wel -- come me home!”
  }
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  She for the Trou -- ba -- dour hope -- less -- ly wept,
  Sad -- ly she thought of him when oth -- ers slept:
  \repeat unfold 2 {
    Sing -- ing, “In search of thee, would I might roam,
    Trou -- ba -- dour! Trou -- ba -- dour! come to thy home.”
  }
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Hark! ’twas the Trou -- ba -- dour breath -- ing her name,
  Un -- der the bat -- tle -- ment soft -- ly he came:
  \repeat unfold 2 {
    Sing -- ing, “From Pal -- es -- tine hith -- er I come,
    La -- dy love! la -- dy love! wel -- come me home!”
  }
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees4 ees8 ees |
  ees ees ees4 |
  d4 c8 d |
  ees4~ ees8 s |
  
  ees4 ees8 ees |
  ees ees ees4 |
  d4 d8 d |
  ees4 s |
  
  \repeat unfold 2 {
    aes4 f8 ees |
    g ees ees4 |
    d4 d8 f |
    ees4 s |

    ees8 d ees4 |
    c8 ees ees4 |
    aes8 f4 d8 |
    ees4 s |
  }
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
  bes4 g8 bes |
  g g g4 |
  bes4 bes8 bes |
  bes4( g8) s8 |

  bes4 g8 bes |
  g g g4 |
  bes4 bes8 aes |
  g4 s |

  \repeat unfold 2 {
    c4 bes8 aes |
    bes bes g4 |
    aes4 aes8 bes |
    bes4 s |

    g8 aes bes4 |
    aes8 g aes4 |
    d8 bes4 aes8 |
    g4 s |
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 ees8 ees8 |
  g f ees4 |
  bes4 bes8 bes8 |
  ees4~ ees8 d8\rest |
  ees4 ees8 ees8 |
  g f ees4 |
  bes4 bes8 bes8 |
  ees4 d\rest |

  \repeat unfold 2 {
    aes4 bes8 c |
    ees ees ees4 |
    bes4 bes8 d |
    ees4 d\rest |
    ees8 bes ees4 |
    aes,8 ees' aes,4 |
    bes8 d4 bes8 |
    ees4 d\rest
  }
  \bar"|."
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


