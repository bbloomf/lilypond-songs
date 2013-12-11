\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Carrol"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
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
       (padding . 2)
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
  \key g\major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 g'4 |
  d' d e8 fis |
  g2 b,4 |
  c8.[ d16 e8 d c b] |
  a2 b4 |

  \slurDashed d( c) b |
  \slurSolid e( fis) g |
  d( c) b |
  a2 a4 |
  \time 6/4
  g2.~ g2
  \repeat volta 2 {
    d'4 |

    e2 fis4 g2 fis4 |
    e( d) c b2 b4 |
    e,2 b'4 c8([ b] a4) g |
    fis2 \bar""

    b4 c2 d4 |
    e2 d4 e8([ fis] g4) b, |
    a2 d4 e8[ d e fis] g4 |
    b, a4.( g8) g2
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Ho -- san -- na to the high -- est Joy __ be -- tide,
  th’Heav’n -- ly Bride -- groom, and his Ho -- ly Bride.

  Let Heav’n a -- bove be fill’d with Songs, let Earth tri -- umph be -- low,
  For ev -- er si -- lent be __ those Tongues, that can __ be si -- lent __ now.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Be rav -- ish’d Earth to see this Con -- tract driv’n,
  Twixt sin -- ful __ Man and rec -- on ciled Heav’n;
  Dis -- mount you Choirs of An -- gels come,
  With Men your Joys div -- ide;
  Heav’n nev -- er show’d so sweet a Groom,
  Nor Earth __ so fair a __ Bride.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  All Glo -- ry be to God, to Man __ good Will,
  \set ignoreMelismata = ##t God sent \unset ignoreMelismata his Son, his Prom -- ise to ful -- fill; __
  Let all Man -- kind be fill’d with Mirth,
  Their joy -- ful Ac -- cents raise,
  And join with An -- gels at __ his Birth,
  To sing __ a Sav -- iour’s __ Praise.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 |
  g fis g8 a |
  b2 g4 |
  e2 g4 |
  fis2 d4 |

  \slurDashed g( a) d, |
  \slurSolid cis( d) e |
  d( e8[ fis]) g4 |
  fis2 fis4 |
  \time 6/4
  g2.~ g2 g4 |

  g2 d4 d( e) fis |
  g2 a4 g2 d4 |
  e2 fis4 g( a ) b |
  a2 \bar""

  g4 e2 d4 |
  g2 f4 e2 g4 |
  fis2 d4 g8[ a b a] g4 |
  g2 fis4 g2 
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
  
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \oneVoice
  g4 |
  b b e,8 d |
  g2 g4 |
  a8.[ b16 c8 b a g] |
  d2 g4 |

  \slurDashed b( a) g8[ fis] |
  \slurSolid e4( d) g |
  b( a) g |
  d2 d4 |
  \time 6/4
  g2.~ g2 g4 |

  c2 d4 g,2 d4 |
  e2 fis4 g2 b,4 |
  c2 d4 e( fis) g |
  d2 \bar""

  g4 a2 b4 |
  c2 b4 a2 g4 |
  d2 b4 c2 c4 |
  d2 d4 g2
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
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
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
