\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Beautiful Dreamer"}}
  composer = \markup\oldStyleNum"Stephen C. Foster"
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##f
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
#(set-global-staff-size 23) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 23 20))) }
global = {
  \key ees \major
  \time 9/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	ees'8 d ees bes4. g |
  f8 e f c'4.~ c |
  bes8 d c c bes aes aes g f |
  g4.~ g~ g | \break

  %page 2
  ees'8 d ees bes4. g |
  f8 e f c'4.~ c |
  bes8 d c c bes aes aes g f |
  ees4.~ ees~ ees | \break

  bes'8 aes f d4. c' |
  c8 bes g ees4.~ ees |

  %page 3
  ees'8 d ees c4. f4 ees8 |
  d ees c bes4.~ bes |
  
  ees8 d ees bes4. g |
  f8 e f c'4.~ c |
  bes8 d c c bes aes aes g f |
  g4.~ g~ g |

  %page 4
  c8 d ees ees bes g aes g f |
  ees4.~ ees~ ees \bar"|."
}
sopWords = \lyricmode {
  \set stanza = "1. "
	Beau -- ti -- ful dream -- er, wake un -- to me, __
  Star -- light and dew -- drops are wait -- ing for thee: __
  
  Sounds of the rude world heard in the day, __
  Lull’d by the moon -- light have all pass’d a -- way! __

  Beau -- ti -- ful dream -- er, queen of my song, __
  List while I woo thee with soft mel -- o -- dy; __

  Gone are the cares of life’s bu -- sy throng,
  Beau -- ti -- ful dream -- er, a -- wake un -- to me! __
  Beau -- ti -- ful dream -- er, a -- wake un -- to me! __
}

sopWordsII = \lyricmode {
  \set stanza = "2. "
  Beau -- ti -- ful dream -- er, out on the sea __
  Mer -- maids are chant -- ing the wild lo -- re -- lei; __

  O -- ver the stream -- let va -- pors are borne, __
  Wait -- ing to fade at the bright com -- ing morn. __

  Beau -- ti -- ful dream -- er, beam on my heart, __
  E’en as the morn on the stream -- let and sea; __
  
  Then will all clouds of sor -- row de -- part, __
  Beau -- ti -- ful dream -- er, a -- wake un -- to me! __
  Beau -- ti -- ful dream -- er, a -- wake un -- to me! __
}

sopWordsIII = \lyricmode {
  \set stanza = "3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = "4. "
  
}

sopWordsV = \lyricmode {
  \set stanza = "5. "
}

altoMusic = \relative c' {
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
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
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
    \tempo 4 = 160
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
      % \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      % \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      % \override VerticalAxisGroup #'staff-staff-spacing =
      % #'((basic-distance . 0)
      %    (minimum-distance . 0)
      %    (padding . -1)
      %    (stretchability . 2))
    }
  }
}
