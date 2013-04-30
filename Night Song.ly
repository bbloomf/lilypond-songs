\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Night Song"}}
  composer = \markup\oldStyleNum"Swedish Folk Song"
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
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 70))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #44
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
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4\p f8. g16 f8. g16 |
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. bes16 g8. aes16 |
  f4 f8. e16 c4 |
  
  f4\p f8. g16 f8. g16 |
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. bes16 g8. aes16 |
  f4 f8. f16 f4 |
  
  \repeat volta 2 {
    c'4^\mf c8. aes16 ees'8. c16 |
    bes4 bes8. aes16 g4 |
    aes aes8. f16 c'8. aes16 |
    g4 g8. e16 c4 |
    
    f4 f8. g16 f8. g16 |
    aes4 aes8. bes16 aes8. bes16 |
    c4^\markup\italic"rall." c8. bes16 g8. aes16 |
    f2 b4\rest |
  }
}
sopWords = \lyricmode {
	Gent -- ly the breez -- es blow through the for -- est;
  Birds voic -- es call -- ing; still is the night.
  Wa -- ters be -- neath them gleam -- ing in moon -- light
  Send back their an -- swers danc -- ing in light.
  
  My dear -- est heart, Oh heark -- en to me!
  Thou art a -- far, my soul cries to thee.
  No an -- swer comes from for -- est or stream -- let;
  Ech -- o but mocks at me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  f4 f8. f16 f8. f16 |
  f4 f8. g16 f8. g16 |
  aes4 aes8. g16 e8. e16 |
  f4 f8. e16 c4 |
  
  f4 f8. f16 f8. f16 |
  f4 f8. g16 f8. g16 |
  aes4 aes8. g16 e8. e16 |
  f4 des8. des16 c4 |
  
  ees4 ees8. ees16 ees8. ees16 |
  ees4 ees8. ees16 ees4 |
  c4 c8. c16 c8. c16 |
  c4 c8. c16 c4 |
  
  f4 f8. f16 f8. f16 |
  e4 e8. e16 e8. e16 |
  e4 e8. e16 e8. e16 |
  c2 s4 |
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
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. c16 c8. c16 |
  c4 c8. c16 bes8. bes16 |
  aes4 aes8. g16 e4 |
  
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. c16 c8. c16 |
  c4 c8. c16 bes8. bes16 |
  aes4 g8. g16 aes4 |
  
  aes4 aes8. aes16 aes8. aes16 |
  g4 g8. g16 bes4 |
  aes aes8. aes16 f8. f16 |
  e4 e8. g16 g4 |
  
  aes4 aes8. bes16 aes8. bes16 |
  aes4 des8. des16 aes8. aes16 |
  aes4 g8. g16 bes8. bes16 |
  aes2 s4 |
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 f8. f16 f8. f16 |
  f4 f8. f16 f8. f16 |
  e4 e8. e16 c8. c16 |
  f4 bes,8. bes16 c4 |
  
  f4 f8. f16 f8. f16 |
  f4 f8. f16 f8. f16 |
  e4 e8. e16 c8. c16 |
  f4 f8. f16 f4 |
  
  aes,4 aes8. aes16 c8. c16 |
  ees4 ees8. ees16 ees4 |
  f f8. f16 aes,8. aes16 |
  c4 c8. c16 e4 |
  
  f4 f8. f16 f8. f16 |
  des4 des8. des16 des8. des16 |
  c4 c8. c16 c8. c16 |
  f,2 d'4\rest |
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


