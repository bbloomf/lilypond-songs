\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smbd Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Go to Dark Gethsemane"}}
  composer = \markup\oldStyleNum"Johann Sebastian Bach (1685–1750)"
  poet = \markup\oldStyleNum"James Montgomery (1771–1854)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 10)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 50))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 35))
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
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	bes'4  a8[ g] d'4 c8[ bes] |
  a4 bes8[ c] fis,2 |
  g8[ a] bes4 ees, d |

  c f d2 |
  bes'4 a8[ g] d'4 c8[ bes] |
  a4 bes8[ c] fis,2 |

  g8[ a] bes4 ees, d |
  c f d2 |
  d'4 f, e f |

  bes a8[ g] a2 |
  a4 bes8[ c] fis,4 d' |
  bes a8[ g] g2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Go to dark Geth -- sem -- a -- ne,
  Ye that feel the tempt -- er’s pow’r;
  Your Re -- deem -- er’s con -- flict see;
  Watch with Him one bit -- ter hour:
  Turn not from His griefs a -- way;
  Learn from Him to watch and pray.

}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  See Him at the judg -- ment -- hall,
  Beat -- en, bound, re -- viled, ar -- raign’d:
  See Him meek -- ly bear -- ing all!
  Love to man His soul sus -- tain’d!
  Shun not suf -- f’ring, shame or loss;
  Learn of Christ to bear the cross.
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Cal -- v’ry’s mourn -- ful moun -- tain view;
  There the Lord of Glo -- ry see,
  Made a sac -- ri -- fice for you,
  Dy -- ing on th’ac -- curs -- ed tree:
  ‘It is fin -- ish’d,’ hear Him cry:
  Trust in Christ, and learn to die.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Ear -- ly to the tomb re -- pair,
  Where they laid his breath -- less clay;
  An -- gels kept their vig -- ils there:
  Who hath tak -- en Him a -- way?
  ‘Christ is ris’n!’ He seeks the skies;
  Sav -- iour! teach us so to rise.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 c bes8[ c] d4 |
  ees d8[ c] d2 |
  d4 d c8[ a] bes4 |

  bes a bes2 |
  d4 c bes8[ c] d4 |
  ees d8[ c] d2 |

  d4 d c8[ a] bes4 |
  bes a bes2 |
  bes4 bes c c |

  d c c2 |
  c4 c d d |
  d c8[ bes] bes2 \bar"|."
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
  g4 fis g g |
  g fis8[ g] a2 |
  g4 g g f |

  f f f2 |
  g4 fis g g |
  g fis8[ g] a2 |

  g4 g g f |
  f f f2 |
  f4 f g f |

  f e f2 |
  f4 g a g |
  g fis g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g,4 a bes g |
  c d8[ ees] d2 |
  bes8[ a] g4 c d8[ ees] |

  f4 f, bes2 |
  g4 a bes g |
  c d8[ ees] d2 |

  bes8[ a] g4 c d8[ ees] |
  f4 f, bes2 |
  bes4 d c8[ bes] a4 |

  g4 c f,2 |
  f'4 ees? d8[ c] bes[ c] |
  d4 d g,2 \bar"|."
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
    \tempo 4 = 85
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
