\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Darling Nelly Gray"}}
  composer = \markup\oldStyleNum"Benjamin R. Hanby (1833–1867)"
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
       (padding . -1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 75))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
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
	\partial 4
  g'8.[ aes16] |
  bes8[ bes] bes[ c] bes g f ees |
  aes bes c d ees4 d8[ c] |
  bes4 bes8[ bes] c bes g ees |
  d[ f] b4\rest b\rest b8\rest g16[ aes] |

  bes8[ bes] bes c bes g f ees |
  aes bes c d ees4 d8[ c] |
  bes4 g8[ bes] bes aes f d |
  ees2 b'4\rest \bar"||"\break

  %chorus
  f8. g16 |
  aes8[ aes] aes8 aes aes4 bes8 aes |
  aes g g g g4 g8 aes |
  bes bes bes bes c bes g ees |
  f2. b8\rest g16 aes |

  bes8 bes bes c bes g f ees |
  aes bes c d ees4\fermata d8[ c] |
  bes4 g8 bes bes aes f d |
  ees2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  There’s a low _ green _ val -- ley on the old Ken -- tuck -- y shore,
  There I’ve whiled ma -- ny hap -- py hours a -- way, _
  A -- _ sit -- ting and a -- sing -- ing by the lit -- tle cot -- tage door,
  Where _ lived my _ dar -- ling Nel -- ly Gray.
  
  \set stanza = #"1-4. "
  Oh! my poor _ Nel -- ly Gray, they have tak -- en you a -- way,
  And I’ll nev -- er see my dar -- ling a -- ny more,
  I’m a -- sit -- ting by the riv -- er and I’m weep -- ing all the day,
  For you’ve gone from the old Ken -- tuck -- y shore.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  When the moon had climb’d the moun -- tain and the stars were shin -- ing too,
  Then I’d take _ my dar -- ling Nel -- ly Gray, _
  And we’d float _ down the riv -- er in my lit -- tle red ca -- noe,
  While my ban -- jo _ sweet -- ly I would play.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  One _ night I went to see her, but, “she’s gone!” the neigh -- bors say,
  The _ white man _ bound her with his chain; _
  They have tak -- en her to Geor -- gia for to wear her life a -- way,
  As she tolls in the cot -- ton and the cane.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  My ca -- noe is un -- der wa -- ter and my ban -- jo is un -- strung,
  I’m _ tired _ of liv -- ing a -- ny more, _
  My _ eyes _ shall look down -- ward, and my song shall be un -- sung,
  While I stay on the old Ken -- tuck -- y shore.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  My _ eyes are get -- ting blind -- ed, and I can -- not see my way;
  Hark! there’s some -- bod -- y knock -- ing at the door— _
  Oh! I hear the an -- gels call -- ing, and I see my Nel -- ly Gray,
  Fare -- _ well to the old Ken -- tuck -- y shore.

  \set stanza = #"5. "
  Oh! my dar -- ling Nel -- ly Gray, up in heav -- en there they say,
  That they’ll nev -- er take you from me a -- ny more,
  I’m a -- com -- ing, com -- ing, com -- ing, as the an -- gels clear the way,
  Fare -- _ well to the old Ken -- tuck -- y shore.
}

altoMusic = \relative c' {
  ees8.[ f16] |
  g8[ g] g[ aes] g ees d ees |
  ees ees ees ees ees4 ees8[ ees] |
  g4 g8[ g] g g ees ees |
  d4 s2 s8 ees16[ f] |

  g8[ g] g aes g ees d ees |
  ees ees ees ees ees4 ees8[ ees] |
  ees4 ees8[ g] f f d bes |
  bes2 s4 \bar"||"

  %chorus
  d8. ees16 |
  f8[ f] f f f4 g8 f |
  f ees ees ees ees4 ees8 f |
  g g g ees ees ees ees ees |
  d2. s8 ees16 f |

  g8 g g aes g ees d ees |
  ees ees ees ees ees4 ees8[ ees] |
  ees4 ees8 g f f d bes |
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
  bes8.[ bes16] |
  bes8[ ees] ees[ ees] ees bes aes g |
  c8 bes aes bes c4 bes8[ aes] |
  bes4 bes8[ bes] bes bes bes g |
  bes4 s2 s8 bes16[ bes] |
  
  bes8[ ees] ees ees ees bes aes g |
  c8 bes aes bes c4 bes8[ aes] |
  g4 bes8[ ees] d c bes aes |
  g2 s4 \bar"||"

  %chorus
  bes8. bes16 |
  d8[ d] d d d4 d8 d |
  bes bes bes bes8 bes4 bes8 bes |
  bes ees ees g, aes bes bes g |
  bes2. s8 bes16 bes |

  bes8 ees ees ees ees bes aes g |
  c bes aes bes c4 bes8[ aes] |
  g4 bes8 ees d c bes aes g2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8.[ ees16] |
  ees8[ ees] ees[ ees] ees ees ees ees |
  aes, aes aes aes aes4 aes8[ aes] |
  ees'4 ees8[ ees] ees ees ees ees |
  bes4 d4\rest d\rest d8\rest bes16[ bes] |
  
  ees8[ ees] ees ees ees ees ees ees |
  aes, aes aes aes aes4 aes8[ aes] |
  bes4 bes8[ bes] bes bes bes bes |
  ees2 d4\rest \bar"||"

  %chorus
  bes8. bes16 |
  bes8[ bes] bes8 bes bes4 bes8 bes |
  ees ees ees ees ees4 ees8 ees |
  ees ees ees ees aes g ees ees |
  bes2. d8\rest bes16 bes |

  ees8 ees ees ees ees ees ees ees |
  aes, aes aes aes aes4\fermata aes8[ aes] |
  bes4 bes8 bes bes bes bes bes |
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
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 1)
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
