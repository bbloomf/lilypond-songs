\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Good Bye, My Lady Love"}}
  composer = \markup\oldStyleNum"Joseph E. Howard (1878–1961)"
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
  \key f \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \repeat volta 2 {
  	a'2~ |
    a8 d c bes |

    a2~ |
    a8 a g f |
    e2~ |
    e8 g f e |
    d2 |
    
    b'2\rest |
    a2~ |
    a8 d c bes |
    a2~ |
    a8 a b c |

    c4. g8 |
    d'4. g,8 |
    c2~ |
    c4 b\rest |
  } \break 

  c8 c4 d8 |
  c16 d8 c16~ c4 |
  a8 a4 bes8 |
  a16 bes8 a16~ a4 |
  a8 g4 fis8 |

  a8 g4 c,8 |
  f a e' d |
  c a bes b |
  c c4 d8 |

  c16 d8 c16~ c4 |
  a8 a4 bes8 |
  a16 bes8 a16~ a8 a |
  a8 g4 a8 |

  c bes g a |
  f2~ |
  f4 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	So __ you’re going a -- way __
  Be -- cause your heart __ has gone a -- stray,
  And __ you prom -- ised me __
  That you would al -- ways faith -- ful be. __
  

  Good bye, my la -- dy love,
  Fare -- well, my tur -- tle -- dove,
  You are the i -- dol and dar -- ling of my heart,
  But some day you will come back to me,
  And love me ten -- der -- ly,
  So good bye, my la -- dy love, good bye.
}

sopWordsII = \lyricmode {
  Go __ to him you love, __
  And be as true __ as stars a -- bove;
  But __ your heart will yearn, __
  And then some day you will re -- turn. __
}

sopWordsIII = \lyricmode {
  \set stanza = #"2. "
  When __ the dew -- drops fall, __
  ’Tis then your heart, __ I know, will call.
  So __ be -- ware, my dove, __
  Don’t trust your life to some false love. __
}

sopWordsIV = \lyricmode {
  But __ if you must go, __
  Re -- mem -- ber, dear, __ I love you so,
  Sure __ as stars do shine, __
  You’ll think of when I called you mine. __
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat volta 2 {
    f2~ |
    f8 f g g |
    f2~ |
    f8 f e d |
    cis2~ |
    cis8 cis cis cis |
    d2 |
    s2 |

    f2~ |
    f8 f g g |
    f2~ |
    f8 f dis dis |
    e4. e8 |
    f4. f8 |
    c\rest e[ f fis]( |
    g4) s |
  }

  f8 f4 f8 |
  f16 f8 f16~ f4 |
  f8 f4 f8 |
  f16 f8 f16~ f4 |
  
  f8 f4 dis8 |
  e8 e4 c8 |
  f8 a gis gis |
  a a g! f |

  f8 f4 f8 |
  f16 f8 f16~ f4 |
  f8 f4 f8 |
  f16 f8 f16~ f8 f |
  
  f8 f4 f8 |
  e8 e e e |
  c2~ |
  c4 s \bar"\."
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
  \repeat volta 2 {
    a2~ |
    a 8 d d d |
    a2~ |
    a8 f g gis |
    a2~ |
    a8 bes a g |
    f2 |
    s |

    a2~ |
    a8 d d d |
    a2~ |
    a8 d a a |
    c4. c8 |
    b4. b8 |
    a8\rest g[ aes a]( |
    bes4) s |
  }

   a8 a4 bes8 |
   a16 bes8 a16~ a4 |
   c8 c4 d8 |
   c16 d8 c16~ c4 |
   
   b8 b4 c8 |
   c c4 bes!8 |
   a a b b |
   c f, g gis |

   a8 a4 bes8 |
   a16 bes8 a16~ a4 |
   c8 c4 d8 |
   c16 d8 c16~ c8 c |
   
   b b4 b8 |
   c c bes! bes |
   a2~ |
   a4 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat volta 2 {
    d,2~ |
    d8 d g, g |
    d'2~ |
    d8 d d d |
    a2~ |
    a8 g a a |
    d2 |
    d\rest |

    d2~ |
    d8 d g, g |
    d'2~ |
    d8 d fis fis |
    g4( e8) c |
    g4 g |
    c2~ |
    c4 d\rest |
  }

  f8 f4 f8 |
  f16 f8 f16~ f4 |
  f8 f4 f8 |
  f16 f8 f16~ f4 |

  d8 g4 a8 |
  c,8 c4 g'8 |
  f8 f f f |
  f f e f |

  f8 f4 f8 |
  f16 f8 f16~ f4 |
  f8 f4 f8 |
  f16 f8 f16~ f8 f |

  d g4 g8 |
  c,8 c c c |
  <f \tweak #'font-size #'-2 f,>2~ |
  q4 d\rest \bar"|."
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
