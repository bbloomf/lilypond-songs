\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"There’s Music in the Air"}}
  composer = \markup\oldStyleNum"George Frederick Root (1820–1895)"
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
       (padding . -3)
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
	\partial 4
  ees4 |
  c' c c c |
  des2( c) |
  bes8 bes bes bes c4 bes |
  aes2. ees4 |
  c' c c c |
  
  des2( c) |
  bes8 bes bes bes c4 bes |
  aes2. b4\rest |
  \repeat volta 2 {
    f aes des f, |
    ees aes c2 |
    c4 bes g ees |
    
    bes' aes ees2 |
    f4 aes des f, |
    ees aes c c8 c |
    c4 bes g ees |
    aes2.
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	There’s mu -- sic in the air,
  When the in -- fant morn is nigh,
  And faint its blush is seen
  On the bright and laugh -- ing sky.
  Ma -- ny~a harp’s ecs -- tat -- ic sound
  Thrills us with its joy pro -- found,
  While we list, en -- chant -- ed there,
  To the mu -- sic in the air.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  There’s mu -- sic in the air,
  When the noon -- tide’s sul -- try beam
  Re -- flects a gold -- en light
  On the dis -- tant moun -- tain stream.
  When be -- neath some grate -- ful shade
  Sor -- row’s ach -- ing head is laid,
  Sweet -- ly to the spi -- rit there
  Comes the mu -- sic in the air.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  There’s mu -- sic in the air,
  When the twi -- light’s gen -- tle sigh
  Is lost on eve -- ning’s breast,
  As its pen -- sive beau -- ties die:
  Then, O, then, the loved ones gone
  Wake the pure, ce -- les -- tial song;
  An -- gel voi -- ces greet us there
  With the mu -- sic in the air.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  c4 |
  ees ees ees ees |
  f2( ees) |
  des8 des des des ees4 des |
  c2. c4 |
  ees ees ees ees |
  
  f2( ees) |
  des8 des des des ees4 des |
  c2. s4 |
  des f f des |
  c c ees2 |
  ees4 des des des |
  
  c c c2 |
  des4 f f des |
  c c ees ees8 ees |
  ees4 des des des |
  c2.
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
  \partial 4
  aes4 |
  aes aes aes aes |
  aes1 |
  g8 g g g g4 g |
  aes2. aes4 |
  aes aes aes aes |
  aes1 |
  g8 g g g g4 g |
  aes2. s4 |
  aes aes aes aes |
  aes aes aes2 |
  g4 g g g |
  
  aes aes aes2 |
  aes4 aes aes aes |
  aes aes aes aes8 aes |
  g4 g bes g |
  aes2.
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  aes,4 |
  aes aes aes aes |
  aes1 |
  ees'8 ees ees ees ees4 ees |
  aes,2. aes4 |
  aes aes aes aes |
  
  aes1 |
  ees'8 ees ees ees ees4 ees |
  aes,2. d4\rest |
  des des des des |
  aes aes aes2 |
  ees'4 ees ees ees |
  
  aes, aes aes2 |
  des4 des des des |
  aes aes aes aes8 aes |
  ees'4 ees ees ees |
  aes,2.
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
    \tempo 4 = 150
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


