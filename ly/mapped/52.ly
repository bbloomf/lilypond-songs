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
       (padding . -3)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 70))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #52
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
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	e8. e16 e8. cis16 |

  e fis8. a4 |
  fis8. fis16 fis8. e16 |
  fis8. gis16 a8[ b] |
  cis8. cis16 b8. a16 |
  a8. b16 cis4 |

  a8. fis16 fis8. e16 |
  e4 a\rest |
  cis8. cis16 cis8. b16 |
  cis8. d16 e4 |
  b8. b16 b8. a16 |

  b8. cis16 d4 |
  e8. cis16 b8. a16 |
  a8. b16 cis4 |
  a8. fis16 fis8. e16 |
  e4 a\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Scots, wha hae wi’ Wal -- lace bled,
  Scots, wham Bruce has af -- ten led,
  Wel -- come to your gor -- y bed,
  Or to vic -- to -- rie!
  Now’s the day, an’ now’s the hour,
  See the front of bat -- tle lour;
  See ap -- proach proud Ed -- ward’s pow’r,
  Chains an’ sla -- ve -- rie!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Wha would be a trai -- tor knave?
  Wha would fill a cow -- ard’s grave?
  Wha sae base as be a slave?
  Let him turn an’ flee!
  Wha, for Scot -- land’s king and law,
  Free -- dom’s sword would strong -- ly draw,
  Free -- man stand, and free -- man fa’,
  Let him on wi’ me!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  By op -- pres -- sion’s woes an’ pains,
  By your sons in ser -- vile chains,
  We will drain our dear -- est veins,
  But they shall be free!
  Lay the proud u -- sur -- pers low!
  Ty -- rants fall in ev -- ’ry foe!
  Lib -- er -- ty’s in ev -- ’ry blow!
  Let us do or dee!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d8. d16 d8. bes16 |
  d ees8. d4 |
  ees8. ees16 ees8. d16 |
  ees8. ees16 d4 |
  f8. f16 ees8. d16 |

  d8. f16 f4 |
  g8. ees16 ees8. c16 |
  d4 s |
  f8. f16 f8. f16 |
  f8. f16 f4 |

  f8. f16 f8. f16 |
  f8. f16 f4 |
  f8. f16 ees8. d16 |
  d8. f16 f4 |
  g8. ees16 ees8. c16 |
  d4 s \bar"|."
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
  bes8. bes16 bes8. bes16 |
  bes bes8. bes4 |
  bes8. bes16 bes8. bes16 |
  bes8. f16 f4 |
  bes8. bes16 a8. bes16 |

  bes8. a16 bes4 |
  bes8. bes16 bes8. a16 |
  bes4 s |
  bes8. bes16 bes8. a16 |
  bes8. c16 d4 |

  a8. a16 a8. g16 |
  a8. bes16 c4 |
  d8. bes16 a8. bes16 |
  bes8. a16 bes4 |
  bes8. bes16 a8. a16 |
  bes4 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,8. bes16 bes8. bes16 |
  bes' bes8. bes4 |
  ees,8. ees16 ees8. bes16 |
  ees8. c16 bes4 |
  bes'8. bes16 f8. g16 |

  g8. f16 bes,4 |
  ees8. ees16 ees8. f16 |
  bes,4 d\rest |
  bes'8. bes16 bes8. f16 |
  bes8. bes,16 bes4 |

  f'8. f16 f8. f16 |
  f8. f16 f4 |
  bes,8. bes16 f'8. g16 |
  g8. f16 bes,4 |
  ees8. ees16 f8. f16 |
  bes,4 d\rest \bar"|."
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
      \new Voice = "sopranos" { \voiceOne << \global \transpose a bes \sopMusic >> }
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Scots wha hae"}}
  composer = \markup\oldStyleNum"Old Scotch Air"
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
  tagline = ""
}}
global = {
  \key f \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'8 a a bes4 bes8 |
  c[ bes] a g[ a] bes |
  c[ f,] bes a4 g8 |
  f4.~ f4 b8\rest |
  a a a bes4 bes8 |

  c[ bes] a g[ a] bes |
  c[ f,] bes a4 g8 |
  f4.~ f4 c'8 |
  c[ a] c f4 c8 |
  c[ a] c c4 c8 |

  d4 c8 c[ bes] a |
  a4.( g4) b8\rest |
  a a a bes4 bes8 |
  c[ bes] a g[ a] bes |
  c[ f,] bes a4 g8 |
  f4.~ f4 b8\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Drink to me on -- ly with thine eyes,
  And I will pledge with mine,
  Or leave a kiss with -- in the cup,
  And I’ll not ask for wine;
  The thirst that from the soul doth rise,
  Doth ask a drink di -- vine,
  But might I of Love’s nec -- tar sip,
  I would not change for thine.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I sent thee late a ro -- sy wreath,
  Not so much hon -- ’ring thee,
  As giv -- ing it a hope that there
  It could not with -- er’d be;
  But thou there -- on didst on -- ly breathe,
  And sent’st it back to me,
  Since when it grows, and smells, I swear,
  Not of it -- self but thee.
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
  f8 f f e4 e8 |
  f4 f8 e[ f] g |
  f4 f8 f4 e8 |
  f4.~ f4 s8 |
  f f f e4 e8 |

  f4 f8 e[ f] g |
  f4 f8 f4 e8 |
  f4.~ f4 f8 |
  f4 f8 a4 f8 |
  f4 f8 e4 f8 |

  f4 f8 f4 f8 |
  f4.( e4) s8 |
  f f f e4 e8 |
  f4 f8 e[ f] g |
  f4 f8 f4 e8 |
  f4.~ f4 s8 \bar"|."
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
  c8 c c c4 c8 |
  c4 c8 c4 c8 |
  a4 bes8 c4 bes8 |
  a4.~ a4 s8 |
  c c c c4 c8 |
  c4 c8 c4 c8 |
  a4 bes8 c4 bes8 |
  a4.~ a4 a8 |
  a[ c] a c4 a8 |
  a[ c] a g4 a8 |

  bes4 bes8 d4 d8 |
  c4.~ c4 s8 |
  c c c c4 c8 |
  c4 c8 c4 c8 |
  a4 d8 c4 bes8 |
  a4.~ a4 s8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,8 f f g4 g8 |
  a4 f8 c4 e8 |
  f4 d8 c4 c8 |
  f4.~ f4 d8\rest |
  f f f g4 g8 |

  a4 f8 c4 e8 |
  f4 d8 c4 c8 |
  f4.~ f4 f8 |
  f4 f8 f4 f8 |
  f4 f8 c4 f8 |

  bes,4 bes8 bes4 b8 |
  c4.~ c4 d8\rest |
  f f f g4 g8 |
  a4 f8 c4 e8 |
  f4 bes,8 c4 c8 |
  f4.~ f4 d8\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Drink to Me Only With Thine Eyes"}}
  poet = \markup\oldStyleNum"Ben Jonson (1572–1637)"
  tagline = ""
}}
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Gaily the Troubadour"}}
  composer = \markup\oldStyleNum"Thomas Haynes Bayly (1797–1839)"
  tagline = ""
}}


