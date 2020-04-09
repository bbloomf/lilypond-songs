\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Look, Ye Saints, the Sight is Glorious"}}
  composer = \markup\oldStyleNum"William Owen (1813–1893)"
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
       (padding . 5)
       (stretchability . 100))
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
  \key c \major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 2 e4 e |
  a2 b c4 c |
  c2 b \bar"||"
  c4 d |
  e2 d4( c) b b |
  a1 \bar"||"
  e4 e |
  a2 b c4 c |

  c2 b \bar"||"
  c4 d |
  e2 d4( c) b b |
  a1. \bar"||"\break
  a4 b c2 a |
  b4 c d2 b |

  c4 d e2 c4( e) |\break
  f8[ e] d4 e8[ d] c4 d8[ c] b[ a] |
  e'1 \bar"||"
  c4 d |
  e2 d4( c) b b |
  a1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Look, ye saints, the sight is glor -- ious;
  See the Man of Sor -- rows now!
  From the fight re -- turned vic -- tor -- ious,
  Ev -- ’ry knee to Him shall \set associatedVoice = "altos" bow.

  \unset associatedVoice
  \repeat unfold 10 \skip1
  Crowns be -- come the vic -- tor’s brow,
  Crowns be -- come the vic -- tor’s brow.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Crown the Sav -- iour! An -- gels, crown Him;
  Rich the tro -- phies Je -- sus brings;
  On the seat of pow’r en -- throne Him
  While the vault of heav -- en \set associatedVoice = "altos" rings.
  %\dropLyricsXII
  Crown Him! Crown Him!
  %\raiseLyrics
  Crown Him! Crown Him!
  Crown Him! Crown \unset associatedVoice Him!
  Crown the Sav -- iour King of kings,
  Crown the Sav -- iour King of kings.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Sin -- ners in de -- ri -- sion crowned Him
  Mock -- ing thus the Sav -- iour’s claim;
  Saints and an -- gels crowd a -- round Him,
  Own His ti -- tle, praise His \set associatedVoice = "altos" Name:
  \unset associatedVoice
  \repeat unfold 10 \skip1
  Spread a -- broad the vic -- tor’s fame,
  Spread a -- broad the vic -- tor’s fame.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Hark, those bursts of ac -- cla -- ma -- tion!
  Hark, those loud tri -- um -- phant chords!
  Je -- sus takes the high -- est sta -- tion;
  Oh, what joy the sight af -- \set associatedVoice = "altos" fords!
  \unset associatedVoice
  \repeat unfold 10 \skip1
  King of kings, and Lord of lords!
  King of kings, and Lord of lords!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

sopWordsAbove = \lyricmode {
  \repeat unfold 30 \skip1
  Crown Him! Crown Him!
  Crown Him! Crown Him!
  Crown Him! Crown Him! __
}

altoMusic = \relative c' {
  c4 c |
  c( e) e2 e4 e |
  e2 e
  e4 g |
  g2 f4( e) e d |
  c1
  c4 c |
  c( e) e2 e4 e |

  e2 e
  e4 g |
  g2 f4( e) e d |
  c1. |
  c2\rest e4 e e e |
  c2\rest gis'4 gis gis gis |

  e2\rest a4 a a a |
  a4 a a a gis8[ a] e[ a] |
  gis1
  a4 gis! |
  a2 f4( e) e d |
  c1 \bar"|."
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
  a4 a |
  a2 gis a4 a |
  a2 gis
  a4 b |
  c2 c4( a) a gis |
  a1
  a4 a |
  a2 gis a4 a |

  a2 gis
  a4 b |
  c2 c4( a) a gis |
  a1.
  s2 a4 a8[ b] c4 c |
  s2 b4 b8[ c] d4 d |

  s2 c4 c8[ d] e4 e |
  d8[ e] f4 e e e b |
  b1
  c4 b |
  a2 a a4 gis |
  a1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a,4 a |
  a( c) e2 a4 a |
  a2 e
  a4 g |
  c,2 d e4 e |
  a,1
  a4 a |
  a( c) e2 a4 a |

  a2 e
  a4 g |
  c,2 d e4 e |
  a,1. |
  d2\rest a'4 a a a |
  d,2\rest e4 e e e |

  d2\rest a'4 a a c |
  d8[ c] b4 c8[ b] a4 b8[ a] g[ f] |
  e1
  a,4 b |
  c2 d e4 e |
  a,1 \bar"|."
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
   \new Lyrics = "sopAbove" \with {
      \override LyricText #'font-size = #-0.9
      \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = 
        #'((basic-distance . 0)
           (minimum-distance . 0)
           (padding . 0)
           (stretchability . 0))
   }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \context Lyrics = "sopAbove" \lyricsto "sopranos" \sopWordsAbove
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
    \new Lyrics \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \lyricsto "tenors" \tenorWordsII
    \new Lyrics \lyricsto "tenors" \tenorWords
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
