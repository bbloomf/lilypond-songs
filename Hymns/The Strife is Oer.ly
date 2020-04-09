\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The strife is o’er"}}
  composer = \markup\oldStyleNum"William Henry Monk (1823–1889)"
  poet = \markup\concat{\italic"Finita jam sunt prœlia" \oldStyleNum", translated by Francis Pott (1832–1909)"}
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##t
  two-sided = ##t
  inner-margin = 0.75\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #136
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{}
  evenHeaderMarkup = \markup {}
}
#(set-global-staff-size 22) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 22 20))) }
global = {
  \key d \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c'' {
  b4\rest fis fis |
  g2. |
  fis4 a a |
  b2. |
  a4 a d |
  cis2. |
  d \bar "||" \break

  a4 a a |
  b2 a4 |
  a( g) fis |
  a2. \bar"||"

  fis4 fis fis |
  fis2 fis4 |
  fis( e) d |
  e2. \bar"||"

  a4 a a |
  b2 a4 |
  a( g) fis |
  a2. \bar"||"

  b4\rest a d |
  cis2. |
  d \bar"|."
}
sopWords = \lyricmode {
  Al -- le -- lu -- ia!
  Al -- le -- lu -- ia!
  Al -- le -- lu -- ia!

  \set stanza = "1. "
  The strife is o’er, the bat -- tle done;
  Now is the Vic -- tor’s tri -- umph won;
  O let the song of praise be sung:
}

sopWordsII = \lyricmode {
  \repeat unfold 12 { \skip 1 }
  \set stanza = "2. "
  Death’s might -- iest pow’rs have done their worst,
  And Je -- sus hath his foes dis -- persed;
  Let shouts of praise and joy out -- burst:
}

sopWordsIII = \lyricmode {
  \repeat unfold 12 { \skip 1 }
  \set stanza = "3. "
  He closed the yawn -- ing gates of hell;
  The bars from heav’n’s high por -- tals fell;
  Let hymns of praise His tri -- umph tell:
  Al -- le -- lu -- ia!
}

sopWordsIV = \lyricmode {
  \repeat unfold 12 { \skip 1 }
  \set stanza = "4. "
  On the third morn he rose a -- gain,
  Glo -- rious in maj -- es -- ty to reign;
  O let us swell the joy -- ful strain:
}

sopWordsV = \lyricmode {
  \repeat unfold 12 { \skip 1 }
  \set stanza = "5. "
  Lord, by the stripes that wound -- ed thee,
  From death’s dread sting Thy ser -- vants free,
  That we may live, and sing to Thee:
}

altoMusic = \relative c' {
  s4 d d |
  d2. |
  d4 d fis |
  fis2. |
  fis4 d fis |
  a2. |
  fis |

  fis4 fis fis |
  g2 fis4 |
  fis( b,) d |
  e2. |

  d4 d d |
  d2 cis4 |
  d( b) d |
  cis2. |

  cis4 fis e |
  d2 fis4 |
  fis( b,) d |
  e2. |

  s4 a fis |
  e2. |
  fis2. \bar"|."

}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = "2. "
}
altoWordsIII = \lyricmode {
  \set stanza = "3. "
}
altoWordsIV = \lyricmode {
  \set stanza = "4. "
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  s4 a a |
  b2. |
  a4 fis a |
  d2. |
  cis4 d d |
  e2. |
  d2. |

  d4 d d |
  d2 d4 |
  cis( e) d |
  cis2. |

  a4 a a |
  b2 a4 |
  a( g) fis |
  a2. |

  a4 d cis |
  b2 d4 |
  cis( e) d |
  cis2. |

  s4 d a |
  a2. |
  a2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,4\rest d d |
  g,2. |
  d'4 d d |
  b2. |
  fis'4 fis d |
  a'2. |
  d, |

  d4 d d |
  g2 d4 |
  fis( e) b' |
  a2. |

  d,4 d d |
  b2 fis'4 |
  d( e) b |
  a2. |

  fis'4 d d |
  g2 d4 |
  fis( e) b' |
  a2. |

  d,4\rest fis d |
  a2. |
  d \bar"|."
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
    \new Lyrics \with { alignAboveContext = "tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = "tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = "tenors" } \lyricsto "tenors" \tenorWords
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


