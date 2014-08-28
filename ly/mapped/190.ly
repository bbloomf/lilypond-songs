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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #190
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 g'8.[ a16] |
  b4 b8. b16 b4 a8. g16 |
  d'2. \bar"" c8. b16 |
  b4 b8. b16 a4 g8. g16 |

  a2. \bar"" g8. a16 |
  b4 b8. b16 b4 a8. b16 |
  d4( c2) \bar"" c8[ e] |

  d4 a8. c16 b4 a8. a16 |
  g2. \bar"||"
  g8. c16 |
  e2. \times 2/3 {c8[ d] e} |

  d2. \bar"" b8. c16 |
  d4 d8 d c4 b8. b16 |
  a2. \bar"" b8. c16 |

  d2. \times 2/3 {b8[ a] g} |
  c8 e2~ e8 \bar"" e e |
  d4 c8 c b4^\markup\italic"rit." a8. a16 |
  g2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  There’s a land ’mid the stars we are told,
  Where they know not the sor -- row of time,
  Crys -- tal foun -- tains in val -- leys of gold, _
  And _ life is a trea -- sure sub -- lime.


}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Here our gaze can -- not soar to that land,
  But our vi -- sions have told of its bliss,
  And our souls by its breez -- es are fanned, _
  When we faint in the des -- ert of this.

  \unset ignoreMelismata
  ’Tis the  \set associatedVoice = "altos" sweet
  ’Tis the \unset associatedVoice sweet
  by and \set associatedVoice = "altos" by, 
  by and \unset associatedVoice by, 
  ’Tis the land of our God we are told;
  Shall we \set associatedVoice = "altos" meet, 
  shall we \unset associatedVoice meet 
  in that cit -- y?
  ’Tis the beau -- ti -- ful home of the soul.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Oh, the stars in the hea -- vens at night
  Seem to tell where the ran -- somed have trod,
  And the sun from his pal -- ace of light _
  Seems to beam with the smiles of our God.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  Oh, _ then let us cling to His Son,
  All our sor -- rows He’ll help us to bear,
  And when life and its du -- ties are done, _
  He has prom -- ised a crown we shall wear.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d8.[ d16] |
  g4 g8. g16 g4 fis8. e16 |
  d2. g8. g16 |
  g4 g8. g16 fis4 e8. e16 |

  fis2. d8. d16 |
  g4 g8. g16 g4 fis8. g16 |
  g2. g8[ g] |

  g4 fis8. fis16 g4 fis8. fis16 |
  g2. \bar"||"
  b,4\rest |
  e\rest g8. g16 g4 e\rest |

  e\rest g8. g16 g4 g8. fis16 |
  g4 g8 g fis4 g8. g16 |
  fis2. e4\rest |

  e\rest g8. g16 g4 \times 2/3 {d4 d8} |
  e8 g2~ g8 g g |
  g4 g8 g g4 fis8. fis16 |
  g2. \bar"|."
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
  b8.[ c16] |
  d4 d8. d16 d4 d8. d16 |
  b2. e8. d16 |
  d4 d8. d16 c4 b8. b16 |

  d2. b8. c16 |
  d4 d8. d16 d4 d8. d16 |
  b4( c2) c4 |

  b c8. a16 d4 d8. c16 |
  b2. \bar"||"
  s4 |
  s c8. c16 c4 c\rest |

  d\rest b8. b16 b4 d8. d16 |
  d4 d8 d d4 d8. d16 |
  d2. s4 |

  s b8. b16 b4 \times 2/3 {g8[ c] b} |
  g c2~ c8 c c |
  b4 c8 c d4 d8. c16 |
  b2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8.[ g16] |
  g4 g8. g16 g4 d8. d16 |
  g2. g8. g16 |
  g4 g8. g16 d4 e8. e16 |

  d2. g8. g16 |
  g4 g8. g16 g4 d8. d16 |
  g4( e2) e4 |

  d4 d8. d16 d4 d8. d16 |
  g2. \bar"||"
  d4\rest |
  d\rest c8. c16 c4 \times 2/3 {e8[ d] c} |

  g'4 g8. g16 g4 g8. a16 |
  b4 b8 b a4 g8. g16 |
  d2. d4\rest |

  d\rest g8. g16 g4 \times 2/3 {g,8[ a] b} |
  c8 c2~ c8 c c |
  d4 e8 e d4 d8. d16 |
  <g g,>2. \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"There’s a land ’mid the stars"}}
  composer = \markup\oldStyleNum"R. A. Glenn"
  poet = \markup\oldStyleNum"W. P. W."
  tagline = ""
}}
