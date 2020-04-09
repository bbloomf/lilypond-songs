\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Hark! the Song of Jubilee"}}
  poet = \markup\oldStyleNum"James Montgomery (1771–1854)"
  composer = \markup\oldStyleNum"George J. Elvey (1816–1893)"
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
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  b'4. b8 d4 b |
  g a b2 |
  b4. b8 d4 b |
  g a b2 |
  b4. b8 c4 c |
  a a b2 |
  b4 cis d g, |
  fis e d2 |

  fis4. fis8 a4 fis |
  g a b2 |
  b4. b8 d4 b |
  c d e2 |
  e4 e c a |
  d d b2 |
  c4 e d g, |
  b a g2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Hark! the song of ju -- bi -- lee;
  Loud as might -- y thun -- ders roar,
  Or the ful -- ness of the sea,
  When it breaks up -- on the shore:

  Al -- le -- lu -- ia! for the Lord
  God om -- ni -- po -- tent, shall reign;
  Al -- le -- lu -- ia! let the word
  E -- cho round the earth and main.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "

  Al -- le -- lu -- ia! hark the sound
  From the depths un -- to the skies,
  Wakes a -- bove, be -- neath, a -- round,
  All cre -- a -- tion’s har -- mo -- nies;

  See the Vic -- tor’s ban -- ner furl’d,
  Sheath’d His sword: He speaks— ’tis done,
  And the king -- doms of this world
  Are the king -- doms of His Son.

}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "

  He shall reign from pole to pole
  With il -- lim -- it -- a -- ble sway;
  He shall reign, when like a scroll
  Yon -- der heav’ns have passed a -- way:

  Then the end; be -- neath His rod,
  Man’s last en -- e -- my shall fall;
  Al -- le -- lu -- ia! Christ in God,
  God in Christ, is all in all.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4. d8 d4 d |
  e fis g2 |
  d4. d8 d4 d |
  e e dis2 |
  e4. e8 e4 e |
  d d d2 |
  d4 e d e |
  d cis d2 |

  d4. d8 d4 d |
  d c b2 |
  d4. d8 g4 g |
  g f e2 |
  e4 gis a e |
  d fis g2 |
  g4 g g g |
  g fis g2 \bar"|."
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
  g4. g8 a4 b |
  b d d2 |
  g,4. g8 a4 g |
  b c fis,2 |
  g4. g8 a4 a |
  fis fis g2 |
  g4 g a b |
  a4. g8 fis2 |

  a4. a8 fis4 a |
  g4. fis8 g2 |
  g4. g8 b4 d |
  c4. b8 c2 |
  b4 e e c |
  a d d2 |
  c4 c d b |
  d4. c8 b2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4. g8 fis4 g |
  e d g2 |
  g4. g8 fis4 g |
  e c b2 |
  e4. e8 a,4 a |
  d d g,2 |
  g'4 e fis g |
  a4 a, d2 |

  d4. d8 d4 c |
  b a g2 |
  g4. g8 g'4 f |
  e d c2 |
  gis'4 e a a |
  fis d g2 |
  e4 c b e |
  d d g2 \bar"|."
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
