\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Goslings"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Humorous Part-Song for Men’s Voices"}}
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  composer = \markup\oldStyleNum"John Frederick Bridge (1844-1924)"
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
       (padding . 0)
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
  \key c \major
  \time 4/4
  \dynamicUp
  \tempo "Allegro con moto"
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

dynamics = {
  \override DynamicText #'self-alignment-X = #2
  s1.\p s4 \once \override DynamicText #'self-alignment-X = #2.5 s4\f |
  s1. s4 s4\p |
  s1*3 |
  %page2
  s1 |
  s2. \once \override DynamicText #'self-alignment-X = #1.5 s4\pp |
  s2. s4\f |
  s1 |
  s2. s4\p |

  s1 |
  s2. s4\f |
  s1*3 |

  s1*4 |
  s2. \bar"||"

  %page3
  s4\p |
  s1. s4 \once \override DynamicText #'self-alignment-X = #2.5 s\f |
  s1. s4 s\p |
  s1*4 |

  s2. \once \override DynamicText #'self-alignment-X = #1.5 s4\pp |
  s2. s4\f |
  s1 |
  s2. s4\p |
  s1 |

  %page4
  s2. \once \override DynamicText #'self-alignment-X = #2.5 s4\f |
  s1*3 |
  s1*4 |
  s2. \bar"||"

  s4\p |
  s1*2 |
  \once \override DynamicText #'self-alignment-X = #2.5 s1.\f s4 s\p |

  %page5
  s1 |
  s4. \once \override DynamicText #'self-alignment-X = #1.7 s8\sf s2 |
  s1*2 |

  s2. s4\p |
  s2. s4\f |
  s1 |
  s2. s4\p |
  s1 |

  s2 s\< |
  s1\f\> |
  s2\! \once \override DynamicText #'self-alignment-X = #2.5 s2\f |
  s1*3 \bar"|."
}

sopMusic = \relative c' {
	e4^\markup\italic"stac." e8 e dis dis dis dis |
  e4 c b'4\rest g,8 g |
  a4 b8( c) a4 b8( c) |

  e2 b'4\rest e,8^\markup\italic"dolce e legato." f |
  g4 e8 d c4 c |
  c4.( f8) f4 d8 e |
  f4 d8[ c] b4 c8[ d] |

  %page 2
  e2 b'4\rest e,8^\markup\italic"cres. con espress." fis 
  g4 fis8 e dis4 fis4 |
  fis4 e2 e4 |
  b'4 a8 g fis4 g |
  e2. e4 |

  e4^\markup\italic"rall." d g e |
  e d c\fermata c^\markup\italic"tempo a la marcia." |
  c'2 b4. fis8 |
  a4( g) f!( d) |
  \times 2/3 {c8([ d c]} b[ c]) e4 d |

  c2. \set midiInstrument = #"acoustic grand" \times2/3{c8[_\markup\smallCapsOldStyle"Accomp." c c ]} |
    \oneVoice <e' fis c'>2 <dis fis b>4. <fis dis b a>8 |
    << {<a fis dis>4( <g e>) } \\ {b,2} >> <a d f>4 <f a d> |
    << {c'4.\trill b16[ c] s4 d8.[ e16]} \\ {<g, e>2 <g c e>4 <f b>} >> |
    <f b d>2( <e c'>4) \bar"||"\break

  %page3
  \set midiInstrument = #"flute"
  \voiceOne
  g,4^\markup\italic"stac." |
  e'8 e e4 dis dis |
  e4 c b'4\rest g,4 |
  a b8[ c] a4 b8[ c] |
  e2 b'4\rest e,8[ f] |

  g4 e8[ d] c4 c |
  c4.( f8) f4 d8[ e] |
  f4 d8 c b4 c8[ d] |
  e2 b'4\rest e,8[^\markup\italic"crs. con espress." fis] |

  g4 fis8[ e] dis4 fis |
  fis e2 e4 |
  b'-> a8 g fis4 g |
  e2. e4^\markup\italic"rall." |
  e8 e d4 g e |

  %page4
  e4 d c\fermata c8.^\markup\italic"tempo a la marcia." c16 |
  c'2 b4. fis8 |
  a4 g f! d |
  \times2/3 {c8([ d c]} b8[ c] e-.) b'8\rest b\rest d, |

  c2. \set midiInstrument = #"acoustic grand" \times2/3{c8[_\markup\smallCapsOldStyle"Accomp." c c ]} |
    \oneVoice <e' fis c'>2 <dis fis b>4. <fis dis b a>8 |
    << {<a fis dis>4( <g e>) } \\ {b,2} >> <a d f>4 <f a d> |
    << {c'4.\trill b16[ c] s4 d8.[ e16]} \\ {<g, e>2 <g c e>4 <f b>} >> |
    <f b d>2( <e c'>4) \bar"||"

  \set midiInstrument = #"flute"
  \voiceOne
  g,4^\markup\italic"stac." |
  e'4 e4 dis dis8 dis |
  e4 c b'2\rest |
  a,4 b8 c a4 b8[ c] |
  e2 b'4\rest e,8 f |

  %page5
  g4 e8[ d] c4 c |
  c4.-> fis8 fis4 fis |
  f!-> d8 c b4 c8[ d] |
  e2 b'4\rest e,8^\markup\italic"cres. con espress." fis |

  g4 fis8[ e] dis4 fis8 fis |
  fis4 e2 e8 e |
  b'4 a8[^\markup\italic"rall." g] fis4 g |
  e2.\fermata g,4^\markup\italic"Grave." |
  e'2 e |

  e e4 f |
  g2( c) |
  g\fermata g^\markup\italic"Maestoso." |
  f2 e4 d |
  d2.^\markup\italic"molto rall." c4 |
  c1\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = "1. "
	She was a pret -- ty lit -- tle gos -- ling,
  And a gay young gos -- ling he;
  And, “I love you,” he said, \set associatedVoice = "altos" “so dear -- ly;”
  \unset associatedVoice
  And, “I love you too,” said she.

  But, “a -- las! we must part,” He whis -- pered,
  “I’m off to the world so wide;
  But love, don’t fear,
  I’ll come next year
  And make you, and make you my __ lit -- tle bride.”

  \repeat unfold 7 ""
  \set stanza = "2."
  ’Twas Mi -- chael -- mas day at morn -- ing,
  That he came home, once more,
  He met his true \set associatedVoice = "altos" love’s mo -- ther,
  \unset associatedVoice
  And oh! she was weep -- ing sore.

  “Too late, you’ve come,” she whis -- pered,
  “They’ve tak -- en your love a -- way,
  She nev -- er will be your bride, ah, me!
  For she’s go -- ing, she’s go -- ing to be cooked __ to -- day!”

  \repeat unfold 7 ""
  \set stanza = "3. "
  Then up he went to the farm -- house:
  “Where is my love?” he said;
  But the far -- mer’s wife she seized a knife
  And cut off his lit -- tle head.
  And she served him up
  With his true love,
  On a dish so deep and wide,
  So though in life they were part -- ed,
  \set associatedVoice = "altos"
  In death they were side by side.
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
  c4 c8 c c c c c |
  c4 g s g8 g |
  a4 b8[ c] a4 b8[ c] |

  c2 s4 c8 c |
  c4 c8 c c4 c |
  c2 c4 c8 c |
  b4 b b b |

  %page 2
  c2 s4 s |
  s2. dis4 |
  dis4 e2 e4 |
  e e8 e dis4 dis |
  e2. b4 |

  b b c c |
  b b c c |
  e2 dis4. dis8 |
  dis4( e) d( a) |
  g2 b4 b |

  c2. s4 |
  s1*3 |
  s2. \bar"||"

  %page 3
  g4 |
  c8 c c4 c c |
  c g s g |
  a b8[ c] a4 b8[ c] |
  c2 s4 c |

  c c c c |
  c2 c4 c |
  b4 b8 b b4 b |
  c2 s |

  s2. dis4 |
  dis e2 e4 |
  e e8 e dis4 dis |
  e2. b4 |
  b8 b b4 c c |

  %page4
  b b c c8. c16 |
  e2 dis4. dis8 |
  dis4 e d a |
  g2( b8)-. s4 b8 |

  c2. s4 |
  s1*3 |
  s2. \bar"||"

  g4 |
  c c c c8 c |
  c4 g s2 |
  a4 b8 c a4 b8[ c] |
  c2 s4 c8 c |

  %page5
  c4. c8 c4 c |
  c4. c8 c4 c |
  b4 b8 b b4 b |
  c2 s2 |
  s2. dis8 dis |
  dis4 e2 e8 e |
  e4. e8 dis4 dis |
  e2. g,4 |
  c2 c |

  c c4 d |
  c1 |
  c2 c |
  b c4 c |
  c2 b |
  g1 \bar"|."
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
  g4 g8 g fis fis fis fis |
  g4 e s g8 g |
  a4 a a a |

  g2 s4 g8 a |
  bes4 bes8 bes bes4 bes |
  a2 a4 a8 a |
  aes4 aes aes aes |

  %page2
  g2 s |
  s2. b4 |
  a g2 g4 |
  g fis8 e b'4 b |
  g2. g4 |

  g g g g |
  g f e e |
  fis2 fis4. b8 |
  b2 a4( f) |
  e2 g4 f |

  e2. s4 |
  s1*3 |
  s2. \bar"||"

  %page3
  g4 |
  g8 g g4 fis4 fis |
  g e s g |
  a a a a |
  g2 s4 g8[ a] |

  bes4 bes bes bes |
  a2 a4 a |
  aes4 aes8 aes aes4 aes |
  g2 s |

  s2. b4 |
  a g2 g4 |
  g fis8 e b'4 b |
  g2. g4 |
  g8 g g4 g g |

  %page4
  g4 f e e8. e16 |
  fis2 fis4. b8 |
  b4 b a f |
  e2( g8)-. s4 f8 |

  e2. s4 |
  s1*3 |
  s2. \bar"||"

  g4 |
  g g fis fis8 fis |
  g4 e s2 |
  a4 a8 a a4 a |
  g2 s4 g8 a |

  %page5
  bes4. bes8 bes4 bes |
  a4. a8 a4 a |
  aes4 aes8 aes aes4 aes |
  g2 s |

  s2. b8 b |
  a4 g2 g8 g |
  g4 fis8[ e] b'4 b |
  g2. s4 |
  s2. g4 |

  g4 g2 g4 |
  g1 |
  g2 g |
  g g4 g |
  g2 f |
  e1 \bar"|."
}

tenorWords = \lyricmode {
  \repeat unfold 160 ""
  So though they were
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \override DynamicText #'self-alignment-X = #4
  c,4 c8 c c c c c |
  c4 c d4\rest e8 e |
  f4 f f f |

  c2 d4\rest c8 c |
  c4 c8 c c4 c |
  c2 c4 c8 c |
  c4 c c c |

  %page2
  c2 d2\rest |
  d\rest d4\rest b4 |
  b c2 c4 |
  b b8 b b4 b |
  e2. e4 |

  f f e c |
  g g a\fermata a |
  a2 b4. b8 |
  e2 f |
  g g,4 g |
  c2. d4\rest |
  \oneVoice
  \set midiInstrument = #"acoustic grand"
  <a a'>2 <b b'>4. q8 |
  <e e,>2 <f f,>4 q |
  <g g,>2 q4 <g, g,> |
  q2( <c c,>4) \bar"||"

  %page 3
  \voiceTwo
  \set midiInstrument = #"flute"
  g'4 |
  c,8 c c4 c c |
  c c d4\rest e |
  f f f f |
  c2 d4\rest c |

  c c c c |
  c2 c4 c |
  c c8 c c4 c |
  c2 d2\rest |

  d2\rest d4\rest b |
  b c2 c4 |
  b b8 b b4 b |
  e2. e4 |
  f8 f f4 e c |

  %page5
  g4 g a\fermata a8. a16 |
  a2 b4. b8 |
  e4 e f f |
  g2( g,8)-. d'\rest d\rest g, |

  c2. d4\rest |
  \oneVoice
  \set midiInstrument = #"acoustic grand"
  <a a'>2 <b b'>4. q8 |
  <e e,>2 <f f,>4 q |
  <g g,>2 q4 <g, g,> |
  q2( <c c,>4) \bar"||"
  \voiceTwo
  \set midiInstrument = #"flute"

  g'4 |
  c, c c c8 c |
  c4 c d2\rest |
  f4-> f8 f f4 f |
  c2 d4\rest c8 c |

  %page5
  c4. c8 c4 c |
  c4.-> c8 c4 c |
  c-> c8 c c4 c |
  c2 d2\rest |

  d2\rest d4\rest b8 b |
  b4 c2 c8 c |
  b4. b8 b4 b |
  e2.\fermata d4\rest |
  d2\rest d4\rest g,_\p |

  c g'2 f4 |
  e1 |
  e2\fermata e |
  d c4 c |
  g'2 g, |
  c1\fermata \bar"|."
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
      \new Voice = "dynamics" { \dynamics }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
    \new Lyrics = "tenorBass"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "tenorBass" \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 120
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
