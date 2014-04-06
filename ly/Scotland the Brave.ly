\version "2.14.2"
\include "util.ly"
\header {
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Scotland the Brave"}}
  composer = \markup\oldStyleNum"19th Century Scottish Folk Song"
  poet = \markup\oldStyleNum"Cliff Hanley"
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
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
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
  \key d\major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \mergeDifferentlyDottedOn
}

sopMusic = \relative c' {
  \repeat unfold 3 {
    d2 d4. e8 |
    fis4 d fis a |
    d2 d4. cis8 |
    d4 a fis d |
    g2 b4. g8 |
    fis4 a fis d |
    e2
  }
  \alternative {
    {
      a4. a8 |
      a1 |
    }
    {
      d,4. d8 d2. b'4\rest \bar"||"\break

      e2 e4. dis8 |
      e4 cis a2 |
      d fis4. e8 |
      d4 b a2 |
      d d4. d8 |
      cis2 d4 cis |
      b d cis b |
      a1
    }
    {
      d,4. d8 d2. b'4\rest \bar"|."
    }
  }

}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Hark! When the night is fall -- ing,
  Hear! Hear, the pipes are call -- ing,
  Loud -- ly and proud -- ly call -- ing, down through the glen.
  There where the hills are sleep -- ing,
  Now feel the blood a -- leap -- ing,
  High as the spi -- rits of the old High -- land men.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  High in the mist -- y High -- lands,
  Out by the pur -- ple is -- lands,
  Brave are the hearts that beat be -- neath Scot -- tish skies.
  Wild are the winds to meet you,
  Staunch are the friends that greet you,
  Kind as the love that shines from fair maid -- ens’ eyes.

  Tow -- ’ring in gal -- lant fame,
  Scot -- land my moun -- tain hame,
  High may your proud stan -- dards glo -- ri -- ous -- ly wave,
  Land of my high en -- deav -- our,
  Land of the shin -- ing riv -- er,
  Land of my heart for -- ev -- er, Scot -- land the brave.

}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Far off in sun -- lit plac -- es,
  Sad are the Scot -- tish fac -- es,
  Yearn -- ing to feel the kiss of sweet Scot -- tish rain.
  Where trop -- ic skies are beam -- ing,
  Love sets the heart a -- dream -- ing,
  Long -- ing and dream -- ing for the home -- land a -- gain.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    a2 b4. b8 |
    d4 d d fis |
    fis2 g4. g8 |
    a4 fis d d |

    b2 d4. b8 |
    d4 d d d |
    b2 
  }
  \alternative {
    {
      e4. e8 |
      d2( cis2) |
    }
    {
      b4. b8 |
      a2. s4 \bar"||"
    }
  }

  a'2 b4. b8 |
  cis4 a e2 |
  fis2 a4. g8 |
  fis4 g e2 |

  fis2 fis4. fis8 |
  fis2 fis4 fis |
  g4 g gis gis |
  a1 |

  d,2 b4. b8 |
  d4 d d fis |
  fis2 fis4. e8 |
  a4 e d d |

  b2 d4. b8 |
  d4 d d d |
  b2 b4. b8 |
  a2. s4 \bar"|."
}
tenorMusic = \relative c' {
  \repeat unfold 2 {
    fis,2 g4. g8 |
    a4 fis a a |
    a2 a4. a8 |
    d4 d a fis |

    d2 g4. d8 |
    a'4 fis a fis |
    d2
  }
  \alternative {
    {
      a'4. a8 |
      a1 |
    }
    {
      g4. g8 |
      fis2. s4 \bar"||"
    }
  }

  cis'2 d4. fis8 |
  e4 e cis2 |
  a2 d4. cis8 |
  d4 d cis2 |

  b2 b4. b8 |
  cis2 a4 a |
  d b b d |
  cis1 |

  a2 g4. g8 |
  a4 fis a a |
  b2 a4. a8 |
  d4 cis a fis |

  d2 g4. d8 |
  a'4 fis a fis |
  d2 g4. g8 |
  fis2. s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat unfold 2 {
    d,2 d4. d8 |
    d4 d d d |
    d2 e4. e8 |
    fis4 d d d |
    
    g,2 g4. g8 |
    d'4 d d d |
    g,2
  }
  \alternative {
    {
      a4. a8 |
      a1 |
    }
    {
      d4. d8 |
      d2. d4\rest \bar"||"
    }
  }

  a'2 a4. a8 |
  a4 a g2 |
  d2 d4. a8 |
  b4 g a2 |
  
  b2 b4. b'8 |
  a2 a4 fis |
  g g e4 e |
  a2( g2) |

  fis2 e4. e8 |
  d4 d d cis |
  b( b') a g |
  fis e d d |

  g,2 g4. g8 |
  d'4 d d d |
  g,2 d'4. d8 |
  d2. d4\rest \bar"|."
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
    \tempo 4 = 195
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
