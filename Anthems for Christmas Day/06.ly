\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Christmas Hymn"}}
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
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 d4 |
  g fis g8.[ a16 b8. c16]( |
  d4) e d8[ c] b4 |
  a4. a8 g4 \bar""

  b |
  d e d c |
  b a b cis |
  d2. \bar""

  c4 |
  b a e d |
  g b8[ d] b4 g |
  a4 \bar""

  d b a |
  e d g8[ a] b[ c] |
  d2 e8[ d c b] |
  a4. a8 g4 \bar"||"

  g4 |
  b a g b |
  c b a b |
  c e d c |
  b8.[ a16 g8. a16 b8. c16]( d4)~ |
  d2. r4 |
  d8[ e16 d c8 d16 c b8 c16 b a8 b16 a]( |
  d,2.) fis4 |
  g2. \bar"|."
}
sopWordsAbove = \lyricmode {
  \repeat unfold 55 ""
  Notes, __
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Re -- joice, re -- joice, __ ye Mor -- tals all re -- joice,
  And pierce the Sky with a me -- lo -- dious Voice.
  No Song so sweet, no Notes too high to sing.
  The great and match -- less Good -- ness of our __ heav’n -- ly King.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Shall An -- gels sing, __ and shell Men si -- lent be;
  Nor im -- i -- tate their Heav’n -- ly Har -- mo -- ny?
  Rouse up my Soul, de -- clare that on this Morn,
  Un -- to Man -- kind on Earth, is a Mes -- si -- ah born.

  O how can Men for -- bear to sing, when Earth with An -- gels \set associatedVoice = "altos" Notes, __
  Notes, __ Notes __ do ring.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Glo -- ry to God, __ and Joy to all a -- bove,
  The Earth is crown’d with Plen -- ty, Men with Love;
  Thus sung the heav’n -- ly Host, in bright Ar -- ray,
  Good Will to Men, the Prince of Peace is __ born this Day.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  r4 |
  r d d2~ |
  d4 g g d |
  d4. c8 b4 \bar""

  d |
  d g fis e8[ fis] |
  g4 a g e |
  d2. \bar""

  e4 |
  e d b d |
  e fis g b, |
  d4 \bar""

  d e d |
  b d g fis |
  g8[ a g fis] e2 |
  d4. c8 b4 \bar"||"

  d4 |
  d d b d |
  e d d d |
  e g fis e |

  d8.[ c16 b8. c16 d8. e16]( d4) |
  g4( fis a d,) |
  d1~ |

  \times2/3{d8[ e d](} \times2/3{g[) fis g](} a4) fis |
  g2. \bar"|."
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
  d,4\rest |
  d\rest d g8.[ fis16 g8. a16]( |
  d,4) c g' g |
  d4. d8 g,4 \bar""

  g' |
  b e,8[ fis] g4 a |
  g fis g a |
  d,2. \bar""

  c4 |
  e fis g fis |
  e d e g |
  d \bar""

  b e fis |
  g fis e d |
  g8[ fis e d] c[ b a g] |
  d'4. d8 g,4 \bar"||"

  g' |
  g d e g |
  c, g d' g |
  e c d e8[ fis] |

  g2( d |
  g8[ a16 g b8 c16 b a8 b16 a g8 a16 g] |
  d1)~ |
  d2. d4 |
  g2. \bar"|."
}
bassWords = \lyricmode {
  \repeat unfold 53 ""
  Notes __
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Lyrics = "sops"
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
    \context Lyrics = "sops" \lyricsto "sopranos" \sopWordsAbove
    \new Lyrics = "basses" \lyricsto "basses" \bassWords
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
