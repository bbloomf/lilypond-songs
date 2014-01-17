\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Drink to Me Only With Thine Eyes"}}
  poet = \markup\oldStyleNum"Ben Jonson (1572–1637)"
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
  \midi {
    \tempo 4 = 80
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
