\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Oh! Susanna"}}
  composer = \markup\oldStyleNum"Stephen C. Foster"
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##f
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
#(set-global-staff-size 22) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 22 20))) }
global = {
  \key g \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 8
  \oneVoice
  g'16[ a]
  b8 d d8. e16 |
  d8 b g8. a16 |
  b8 b a g |
  a4. \break\bar"" g16[ a] |

  b8 d d8. e16 |
  d8 b g8.[ a16] |
  b8 b a a |
  g4. \break\bar"" g16[ a] |

  b8 d d8. e16 |
  %page 2
  d8 b g8. a16 |
  b8 b a g |
  a4 r8 \break\bar"" g16[ a] |
  b8 d d8. e16 |
  d8 b g8. a16 |

  b16 b8. a8. a16 |
  g4 r | \break
  \voiceOne
  \repeat volta 2 {
    c c |
    e8 e4 e8 |
    d d b g |
    a4 b8\rest\break\bar"" g16[ a] |
    b8 d d8. e16 |
    d8 b g8. a16 |
    b8 b a a |
    g4 b\rest
  }
}
sopWords = \lyricmode {
  \set stanza = "1. "
  I __ came from Al -- a -- bam -- a,
  With my ban -- jo on my knee.
  I’m goin' to Loui -- si -- a -- na,
  My __ true love for to see;

  It __ rain’d all night the day I left,
  The wea -- ther it was dry.
  The sun so hot I froze to death,
  Su -- san -- na don’t you cry.
}

sopWordsII = \lyricmode {
  \set stanza = "2. "
  I __ had a dream the oth -- er night,
  When ev -- ’ry -- thing was still,
  I __ thought I saw Su -- san -- na
  A -- com -- ing down the hill;
  
  The buck -- wheat cake was in her mouth,
  The tear was in her eye;
  Says I, “I’m com -- ing from the south,
  Su -- san -- na don’t you cry.”

  Oh! Su -- san -- na,
  Oh don’t you cry for me.
  I’ve come rom Al -- a -- bam -- a
  With my ban -- jo on my knee.
}

sopWordsIII = \lyricmode {
  \set stanza = "3. "
  I __ soon will be in New Or -- leans,
  And then I’ll look all ’round,
  And when I find Su -- san -- na,
  \set ignoreMelismata = ##t I will fall up -- on the ground. \unset ignoreMelismata

  And if I do not find her __ _
  Then I will sure -- ly die,
  And when I’m dead and bur -- ied __ _
  Su -- san -- na don’t you cry.
}

sopWordsIV = \lyricmode {
  \set stanza = "4. "
  
}

sopWordsV = \lyricmode {
  \set stanza = "5. "
}

altoMusic = \relative c' {
  s8 s2*16

  g'4 g |
  g8 g4 g8 |
  g g g g |
  fis4 b8\rest g8 |

  %page 3
  g8 g g8. g16 |
  g8 g g8. g16 |
  g8 g fis fis |
  g4 b\rest
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = "2. "
}
altoWordsIII = \lyricmode {
  \set stanza = "3. "
}
altoWordsIV = \lyricmode {
  \set stanza = "4. "
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  s8 s2*16

  \repeat volta 2 {
    e4 e |
    c8 c4 c8 |
    b b d b |
    d4 d,8\rest d' |

    %page 3
    d b b8. c16 |
    b8 d d8. e16 |
    d8 d c c |
    b4 d,\rest
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  s8 s2*16

  \repeat volta 2 {
    c,4 c |
    c8 c4 c8 |
    g' g g g |
    d4 d8\rest g |

    %page 3
    g g g8. g16 |
    g8 g b,8. c16 |
    d8 d d d |
    g,4 d'\rest
  }
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
    \tempo 4 = 160
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
      % \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      % \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      % \override VerticalAxisGroup #'staff-staff-spacing =
      % #'((basic-distance . 0)
      %    (minimum-distance . 0)
      %    (padding . -1)
      %    (stretchability . 2))
    }
  }
}
