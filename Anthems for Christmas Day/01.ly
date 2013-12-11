\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"An Anthem for Christmas Day"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
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
       (padding . -3)
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
  \key g \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	s2.*5
  b'8.[ c16] |
  d2. |
  b4\rest b\rest b8.[ a16] |
  a2 b4 |
  d d d |
  g, g a |
  a a b\rest |

  %page2 (6)
  b4 g b |
  c8[ b a b a g] |
  fis4. e8 d4 |
  g4. a8 fis4 |
  g g \break
  \repeat volta 2 {
    b\segno |

    d b r |
    r r b8.[ c16]( |
    d8.[) e16 d8. c16 b8. c16]( |
    b8.[) c16 d8. e16 d8. c16] |
    b4 g d' |

    %page3 (7)
    e8[ d c d c b] |
    a4. a8 d4 |
    b4. c8 b[ a] |
  }
  \alternative {
    {
      \partial 2 b4 g
    }
    {
      b4 g2
    }
  }

  \time 2/2
  b4 b8 a g4 d'8 c |
  b8.[ a16 g8] g a4. a8 |
  a4
  \repeat volta 2 {
    d b8 g4 a8 |

    %page4 (8)
    fis8 d4 b'8 c d g, g |
    a16.[ b32 a16 b c8] b a4 d |

    d8 c( b4) c8 b[ a g] |
    fis4. g8
  }
  \alternative {
    {
      \partial 4
      g4
    }
    {
      \time 3/4
      \partial 2 g2^\markup"D.S." |
    }
  }
  
  %{
  \repeat volta 2 {
    b4 |

    d b r |
    r r b8.[ c16]( |
    d8.[) e16 d8. c16 b8. c16]( |
    b8.[) c16 d8. e16 d8. c16] |
    b4 g d' |

    e8[ d c d c b] |
    a4. a8 d4 |
    b4. c8 b[ a] |
  }
  \alternative {
    {
      \partial 2 b4 g
    }
    {
      b4 g2
    }
  }%}
  %page (10)
  \pageBreak\time 2/2
  b2\rest b4\rest r |
  r1 |
  r2 r4

  \repeat volta 2 {
    b8 c |
    d8. c16 b4 c8.[ d16] e8.[ d16] |
    cis4. b8 b2 |

    d8.[ c16] b[ a] g[ a] c8 b( a4) |
  }
  \alternative {
    {
      \partial 2. g2.
    }
    {
      g2 b\rest
    }
  }
  b1\rest |

  %page (11)
  b\rest |
  b\rest |
  b\rest |
  b\rest |
  
  \break
  \repeat volta 2 {
    b4 b8 b d4 g,8 g |
    a4. a8 b4 b8 b |
    d4 d8 d cis4. cis8 |

    %page (12)
    \time 3/2
    d2. d4 g,4( a) |
    b1 b2\rest |
    e1 d2 |
    c b2. b4 |
    b1 b2\rest |

    g1 b2 |
    c d2. d4 |
    b1.
  }
  \time 2/2
  d4 d b g |
  a b8[ c] b4 g |

  %page (13)
  b4. c8 d4 a |
  g8[ b] a[ g] fis4 d |
  a' a d, g |
  fis fis g d |
  d'4. e8 a,4 d |

  d cis d d |
  a a fis d |
  g8[ b] a[ c] b[ a] g4 |
  c8[ d] b[ c] a[ b] g[ a] |
  fis4 g a d, |

  %page (14)
  d'4 d d( c8[ b]) |
  c2 c4 c |
  c( b8[ a]) b2 |
  d4( c8[ b] a4) b8[ c] |
  b4( a) g2 \bar"|."
}
sopWordsAbove = \lyricmode {
  \repeat unfold 28 ""
  glad __
  \repeat unfold 14 ""
  for un -- to you, un -- to you is born this Day
}
sopWords = \lyricmode {
  %\set stanza = #"1. "
  Be -- hold, be -- hold, I bring you, I bring you glad Tid -- ings,

  Tid -- ings of great __ Joy, which shall be to all Peo -- ple, glad Tid -- \set associatedVoice = "altos" ings,
  glad Tid -- ings, glad Tid -- ings, glad Tid -- ings, glad Tid -- ings
  \set associatedVoice = "sopranos"
  of great __ Joy, which shall be to all Peo -- ple; Peo -- ple;

  \set associatedVoice = "tenors" for un -- to you, un -- to you is born this \set associatedVoice = "sopranos" Day, a Sav -- iour, a
  Sav -- iour, a Sav -- iour, which is Christ the Lord,
  a Sav -- iour, which is Christ the Lord,
  \set associatedVoice = "altos" Lord,

  %glad Tid -- \set associatedVoice = "altos" ings,
  %glad Tid -- ings, glad Tid -- ings, glad Tid -- ings, glad Tid -- ings
  %\set associatedVoice = "sopranos"
  %of great __ Joy, which shall be to all Peo -- ple; Peo -- \set associatedVoice = "altos" ple;

  And this shall be a Sign un -- to you.
  \set associatedVoice = "sopranos"
  Ye shall find the Babe wrapt in swad -- ling Cloaths,

  and in a Man -- ger laid. laid.

  “Glo -- ry to God in the high -- est,
  Glo -- ry to God in the high -- est,
  and on Earth Peace, Peace, good Will to -- wards Men.
  Peace, good Will to -- wards Men.”

  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,

  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,

  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  
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
  s2.*5
  d4 |
  d2. |
  s2 g4 |
  fis2 d4 |
  d d d |
  g g e |
  fis d s |

  %page2 (6)
  d4 d d |
  g( g8[ fis] e4) |
  d4. d8 d4 |
  e4. e8 d4 |
  b b d\rest |

  d\rest d\rest g |
  fis d g |
  fis d g |
  fis d d |
  d d d |

  %page3 (7)
  g4~( g8[ fis] e4) |
  d4. d8 d4 |
  e4. e8 d4 |
  b b
  b b2 |

  \time 2/2
  r4 r d d8 d |
  g4 g8 fis e e e e |
  fis2 r |

  %page4 (8)
  r1 |
  r2 r4 fis |
  g8 g4. e16[ fis] g4. |
  d4. c8 b4

  \time 3/4
  b2 %{r4 |
  d\rest d\rest g |

  %page5 (9)
  fis d g |
  fis d g |
  fis d d |
  d d d |

  g4( g8[ fis] e4) |
  d4. d8 d4 |
  e4. e8 d4 |
  b b
  b b2 |
%}
  %page 6 (10)
  \time 2/2
  s2. d4 |
  g8 fis g d e8.[ fis16 e fis] g8 |
  fis2 d4

  d8 d |
  d8. d16 d4 e8.[ fis16] g8.[ fis16] |
  e4. e8 dis2 |

  d4 d8 d e4 d
  b2.
  b2 s |
  s1 |

  %page 7 (11)
  s1*4 |

  d4 d8 d g4 g8 g |
  fis4. d8 g4 g8 g |
  fis4 g8 g a4. a8 |

  %page 8 (12)
  \time3/2
  fis2. fis4 e2 |
  d1. |
  g1 g2 |
  fis4( e) e2 dis |
  e1. |

  g1 d2 |
  e4( g) fis2. fis4 |
  g1. |
  \time2/2
  r1 |
  r1 |

  %page 9 (13)
  g4 e fis d |
  e a, d d |
  fis fis g d |
  c d d b |
  g' g a fis |

  g e d a |
  r1 |
  g'4 fis g d |
  r1 |
  r1 |

  %page10 (14)
  g4 g g( fis) |
  e2 e4 e |
  fis( e) d2 |
  g( fis4) g |
  g( fis) g2 \bar"|."
}
altoWords = \lyricmode {
  \repeat unfold 49 ""
  for un -- to you, un -- to you is born this Day,
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
  s2.*5
  g8.[ a16] |
  b2. |
  s2 b8.[ c16] |
  d2 d4 |
  b b b |
  e e cis |
  d d s |

  %page2 (6)
  d4 b b |
  e8[ d c d c b] |
  a4. a8 d4 |
  b4. c8 b[ a] |
  b4 g g |

  b g r |
  r r b8.[ c16]( |
  d8.[) e16 d8. c16 b8. c16]( |
  b8.[) c16 d8. e16 d8. c16] |
  b4 g b |

  %page3 (7)
  c8[ b a b a g] |
  fis4. e8 d4 |
  g4. a8 fis4 |
  g g
  g g2 |

  \time 2/2
  d'4 d8 c b4 b8 a |
  g4 d' d cis |
  d2 s |

  %page4 (8)
  s1 |
  s2. a4 |

  b16[ c] d4. e8 d[ c b] |
  a4. g8 g4

  \time 3/4
  g2 %{ g4 |
  b g r |

  %page5 (9)
  r r b8.[ c16]( |
  d8.[) e16 d8. c16 b8. c16]( |
  b8.[) c16 d8. e16 d8. c16] |
  b4 g b |

  c8[ b a b a g] |
  fis4. e8 d4 |
  g4. a8 fis4 |
  g g
  g g2 |
%}
  %page6 (10)
  s1*3 |

  s1*2 |

  s1
  s2. s1 |
  s1 |

  %page7 (11)
  s1*4 |

  g4 g8 g b4 b8 b |
  d4. d8 d4 d8 d |
  d4 d8 d e4. e8 |

  %page8 (12)
  \time 3/2
  d2. b4 c2 |
  b1. |
  c1 b2 |
  a4( g) g2. fis8[ e] |
  e1 s2 |

  e'1 d2 |
  c4( b) a2. a4 |
  g1. |
  \time 2/2
  s1*2 |

  %page9 (13)
  s1*2 |
  d'4 d b g |
  a b8[ c] b4 g |
  b4. c8 d4 a |

  g8[ b] a[ g] fis4 d |
  r1 |
  d'4 d d b |
  r1 |
  r1 |

  %page10 (14)
  d4 d d( c8[ b]) |
  c2 c4 c |
  c( b8[ a]) b2 |
  d4( e d) d |
  d2 b \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4 g,8.[ a16] |
  b2. |
  d4\rest d\rest d |
  g2 g4 |
  b b a16.[ b32 a16 g] |
  fis4 d \bar""

  g8.[ fis16] |
  g2. |
  d4\rest d\rest g, |
  d'2 d4 |
  g g g |
  e e a, |
  d d d\rest |

  %page2 (6)
  g g g |
  c,8[ d e d e c] |
  d4. c8 b4 |
  e4. c8 d4 |
  g, g b\rest |

  b\rest b\rest g' |
  a d, g8.[ a16]( |
  b8.[) c16 b8. a16 g8. a16(] |
  g8.[) a16 b8. c16 b8. a16] |
  g4 g g |

  %page3 (7)
  c,8[ d e d e c] |
  d4. c8 b4 |
  e4. c8 d4 |
  g, g
  g g2 |

  \time 2/2
  d'4\rest d\rest g4 g8 fis |
  e4 d a' a, |
  d2 d\rest |

  %page4 (8)
  d1\rest |
  d2\rest d4\rest d |

  g8 g4. c8 c,4. |
  d4. d8 g,4

  \time 3/4
  g2 %{b4\rest |
  b\rest b\rest g' |

  %page5 (9)
  a d, g8.[ a16]( |
  b8.[) c16 b8. a16 g8. a16(] |
  g8.[) a16 b8. c16 b8. a16] |
  g4 g g |

  c,8[ d e d e c] |
  d4. c8 b4 |
  e4. c8 d4 |
  g, g
  g g2 |
%}
  %page6 (10)
  \time 2/2
  d'1\rest |
  d\rest |
  d2\rest d4\rest

  \oneVoice
  g8 a |
  b8. a16 g8.[ fis16] e8.[ d16] c8.[ d16] |
  e8[ fis] fis,4 b2 |

  b'8.[ a16] g[ fis] e[ d] c4 d
  g,2.

  g2. d'4 |
  b8 b b b e8.[ fis16] fis8[ d] |

  %page7 (11)
  fis8.[ g16 a8] a, d4. b8 |
  c8.[ d16 c8. d16 e8. d16 e8. fis16]( |
  g8.[) a16 g8. fis16] g8.[ a16 b8. a16] |
  g4. fis8 g4 g |

  \voiceTwo
  g,4 g8 g g'4 e8 e |
  d4. d8 b'4 b8 b |
  b4 b8 b a4. a8 |

  %page8 (12)
  \time 3/2
  d,2. d4 e( fis) |
  g1 d2\rest |
  c1 g2 |
  a b2. b4 |
  e1 d2\rest |

  c'1 b2 |
  a4( g) d2. d4 |
  g,1. |
  \time 2/2
  d'1\rest |
  d1\rest |

  %page9 (13)
  d1\rest |
  d1\rest |
  r1 |
  r1 |
  g4 e fis d |

  e a, d d |
  cis a d c |
  b d g g, |
  e' d c b |
  a g d' d |

  %page10 (14)
  b4 b e( fis8[ g]) |
  a2 a,4 a |
  d( e8[ fis]) g2 |
  b,4( c d) g |
  d2 g, \bar"|."
}
bassWords = \lyricmode {
  Be -- hold, be -- hold, I bring you glad Tid -- ings,
  \repeat unfold 28 ""
  glad __
  \repeat unfold 14 ""
  for un -- to you is born this Day,
  \repeat unfold 26 ""
  and sud -- den -- ly ap -- pear’d the Heav’n -- ly Host, thus prais -- ing __ God, and say -- ing,
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Lyrics = "sopAbove"
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altoLyrics" \lyricsto "altos" \altoWords
    \new Lyrics = "altos"
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "sopAbove" \lyricsto "sopranos" \sopWordsAbove
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
    \new Lyrics \lyricsto "basses" \bassWords
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
