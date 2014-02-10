\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Scots wha hae"}}
  composer = \markup\oldStyleNum"Old Scotch Air"
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
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
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 70))
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
