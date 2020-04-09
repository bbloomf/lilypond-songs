\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Forty Days and Forty Nights"}}
  composer = \markup\oldStyleNum"Martin Herbst (1654–1681)"
  poet = \markup\oldStyleNum"George Hunt Smyttan (1822–1870)"
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'4 a d, e |
  f g a2 |
  a4 a c c |
  b b a2 |

  a4 a bes? bes |
  g g a2 |
  a4 a g f |
  e e d2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  For -- ty days and for -- ty nights
  Thou wast fast -- ing in the wild;
  For -- ty days and for -- ty nights
  Tempt -- ed, and yet un -- de -- filed.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Shall not we Thy sor -- row share,
  And from earth -- ly joys ab -- stain,
  Fast -- ing with un -- ceas -- ing prayer,
  Glad with Thee to suf -- fer pain?
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  And if Sa -- tan vex -- ing sore,
  Flesh or spi -- rit should as -- sail,
  Thou, his Van -- quish -- er be -- fore,
  Grant we may not faint or fail.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  So shall we have peace div -- ine;
  Ho -- lier glad -- ness ours shall be;
  Round us, too, shall an -- gels shine,
  Such as min -- i -- ster’d to Thee.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  Keep, O keep us, Sav -- iour dear,
  Ev -- er con -- stant by Thy side;
  That with Thee we may ap -- pear
  At th’e -- ter -- nal Ea -- ster -- tide.
}

altoMusic = \relative c' {
  d4 d a a |
  a d cis2 |
  d4 d e e |
  e e cis2 |

  d4 d d d |
  c c c2 |
  f4 f e d |
  d cis d2 \bar"|."
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
  f,4 f f e |
  d d e2 |
  f4 f a a |
  a gis a2 |

  f4 f g g |
  e e f2 |
  c'4 c c a |
  a e f2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,4 d d cis |
  d b a2 |
  d4 d a a |
  e' e a,2 |

  d4 d g, g |
  c c f,2 |
  f'4 f c d |
  a a d2 \bar"|."
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
