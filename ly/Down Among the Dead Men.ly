\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Down Among the Dead Men"}}
  poet = \markup\oldStyleNum"Sir Edward Dyer (1543–1607)"
  composer = \markup\oldStyleNum"Late 17th Century English Folk Song"
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
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \relative c' {
	\partial 4 g'8[ g] |
  c4~ c8 b8 c4~ c8 g |
  aes4 f g4. g8 |
  aes4~ aes8 g8 f4 ees |

  bes'4 bes, ees g |
  c4. b8 c4 g |
  aes g8[ f] g4. g8 |
  aes4. g8 f4 ees |

  bes'4 bes, ees4. g8 |
  c4. b8 c4 d |
  ees4. d8 ees2 |
  ees,8 f g aes bes4 bes |

  f8 g aes bes c4 c |
  c2 bes |
  aes g |
  ees'8 d c b c4 f, |
  g g c, \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	Here’s a health to the King, and a last -- ing peace,
  To fac -- tion an end, to wealth in -- crease;
  So come, let’s drink it while we have breath,
  For there’s no drink -- ing af -- ter death,

  And he that will this health de -- ny,
  Down a -- mong the dead men,
  down a -- mong the dead men,
  Down, down, down, down,
  Down a -- mong the dead men let him lie!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Let _ charm -- _ ing bea -- _ ty’s health go round,
  In whom _ ce -- les -- tial joys are found,
  And may con -- fu -- sion still pur -- _ sue
  The self -- ish wo -- man hat -- ing crew;

  And they that wo -- man’s health de -- ny,
  Down a -- mong the dead men,
  down a -- mong the dead men,
  Down, down, down, down,
  Down a -- mong the dead men let them lie!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8[ g] |
  ees4~ ees8 g c,4~ c8 c |
  f4 f d4 ees |
  f4~ f8 c8 c4 c |
  ees d ees f |

  ees4. g8 c,4 c |
  f4 d8[ f] d4. g8 |
  aes4. g8 f4 ees |
  ees d ees4. g8 |

  ees4. d8 ees4 aes |
  g4. f8 g2 |

  ees8 f ees f f4 f |
  f8 ees f f g4 g |
  f2 e |
  f d |
  ees8 g ees d ees4 c |
  d d c4 \bar"|."
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
  g8[ g] |
  g4~ g8 g g4~ g8 g |
  c4 c b g |
  c4~ c8 g aes4 g8[ aes] |
  f4 f g b |
  
  c4. g8 g4 g |
  c b8[ c] b4. g8 |
  aes4. g8 f4 ees |
  f4 f g4. g8 |

  g4 g g f |
  bes4 bes bes2 |
  
  c8 c c c d4 d |
  c8 c c c ees4 ees |
  c2 c |
  c b |
  c8 b g g g4 aes |
  c b g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8[ g] |
  c,4~ c8 d8 ees4~ ees8 ees |
  f4 aes g c, |
  f4~ f8 ees d4 c8[ aes] |
  bes4 bes ees d |
  
  c4. d8 ees4 ees |
  f g8[ aes] g4. g8 |
  aes4. g8 f4 ees |
  bes bes ees4. g8 |

  c,4 g c bes |
  ees bes ees4.( d8) |
  c8 c c c  bes8[ c] d[ f] |
  aes8 g f f c[ d] ees[ g] |
  aes[ g aes f] g[ f g c,] |
  f[ e f d] g[ fis g g,] |
  c8 g c g c4 aes |
  g g c \bar"|."
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
