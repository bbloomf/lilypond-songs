\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Savior, when in dust to Thee"}}
  composer = \markup\oldStyleNum"17th Century Spanish Melody"
  poet = \markup\oldStyleNum"Robert Grant (1785–1838)"
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
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\repeat unfold 2 {
    aes'4. aes8 g4 aes |
    f aes ees2 |
    c'4 aes des bes |
    aes g aes2 |
  }
  c4. c8 bes4 c |
  des c bes2 |
  c4. c8 bes4 c |
  des c bes2 |

  aes4. aes8 g4 aes |
  f aes ees2 |
  c'4 aes des bes |
  aes g aes2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Sav -- ior, when in dust to Thee
  Low we bow th’a -- dor -- ing knee;
  When, re -- pent -- ant, to the skies
  Scarce we lift our weep -- ing eyes;
  Oh! by all Thy pains and woe
  Suf -- fered once for man be -- low,
  Bend -- ing from Thy throne on high,
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  By Thy birth and ear -- ly years,
  By Thy hu -- man griefs and fears,
  By Thy fast -- ing and dis -- tress
  In the lone -- ly wil -- der -- ness,
  By Thy vic -- t’ry in the hour
  Of the sub -- tle tempt -- er’s pow’r,
  Je -- sus, look with pit -- ying eye;
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  By Thy con -- flict with de -- spair,
  By Thine a -- go -- ny of prayer,
  By the pur -- ple robe of scorn,
  By Thy wounds, Thy crown of thorn,
  By Thy cross, Thy pangs and cries,
  By Thy per -- fect sac -- ri -- fice,
  Je -- sus, look with pit -- ying eye;
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  By Thy deep ex -- pir -- ing groan,
  By the seal’d se -- pul -- chral stone,
  By Thy tri -- umph o’er the grave,
  By Thy pow’r from death to save;
  Might -- y God, as  --  cend  --  ed Lord,
  To Thy throne in heav’n re -- stored,
  Prince and Sav -- ior, God most high,
  Hear our pen -- i -- ten -- tial cry!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    ees4. ees8 ees4 ees |
    des des c2 |
    ees4 ees f f |
    ees des c2 |
  }

  ees4. ees8 ees4 ees |
  ees ees ees2 |
  ees4. ees8 ees4 ees |
  ees ees ees2 |

  ees4. ees8 ees4 ees |
  des des c2 |
  ees4 ees f f |
  ees des c2 \bar"|."
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
  \repeat unfold 2 {
    c4. c8 des4 c |
    aes aes aes2 |
    aes4 aes aes des |
    c bes aes2
  }

  \repeat unfold 2 {
    aes4. aes8 g4 aes |
    bes aes g2 |
  }

  c4. c8 des4 c |
  aes aes aes2 |
  aes4 aes aes des |
  c bes aes2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat unfold 2 {
    aes4. aes8 bes4 aes |
    des, f aes2 |
    aes,4 c des des |
    ees ees aes,2 |
  }

  \repeat unfold 2 {
    aes'4. aes8 ees4 aes |
    g aes ees2 |
  }

  aes4. aes8 bes4 aes |
  des, f aes2 |
  aes,4 c des des |
  ees ees aes,2 \bar"|."
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
