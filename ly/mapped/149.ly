\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
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
  first-page-number = #149
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
  \key aes \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  
}

sopMusic = \relative c' {
	\partial 8
  aes'8 aes8. bes16 aes8 aes8. bes16 aes8
  aes8 b\rest ees ees b\rest c |
  c8. bes16 aes8 bes8 ees, bes' |
  aes4. b4\rest \bar""
  
  aes16~ aes |
  aes8. bes16 aes8 aes8. bes16 aes8 |
  aes8 b\rest ees ees b\rest c |
  c8. des16 c8 c8. des16 c8 |
  c b\rest f' f b,\rest \bar""
  
  c16 c |
  bes8. c16 bes8 bes8. c16 bes8 |
  ees4. ees,4 c'8 |
  bes8. c16 bes8 bes8. c16 bes8 |
  ees,4. b'4\rest \bar""
  
  ees,8 |
  f g aes des c des |
  bes aes bes g4 ees8 |
  f g aes des c des |
  bes aes bes g4\fermata \bar""\break
  
  ees8 |
  aes8. aes16 aes8 aes8[ ees'] ees, |
  aes8. aes16 aes8 aes8[ ees'] ees,8 |
  c'8. bes16 aes8 bes g ees |
  aes4. b4\rest \bar""
  
  ees,8 |
  aes8. aes16 aes8 aes[ ees'] ees, |
  aes8. aes16 aes8 aes8( aes'4) |
  bes,8. c16 des8 bes4 ees8 |
  aes,4. b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set ignoreMelismata = ##t
  The flow -- ers that bloom in the spring, Tra la,
  Breathe prom -- ise of mer -- ry sun -- shine,
  As we mer -- ri -- ly dance and we sing, Tra la,
  We wel -- come the hope that they bring, Tra la,
  Of a sum -- mer of ros -- es and wine,
  Of a sum -- mer of ros -- es and wine;
  
  And that’s what we mean when we say that a thing
  Is wel -- come as flow -- ers that bloom in the spring.
  
  \unset ignoreMelismata
  Tra la la la la,
  Tra la la la la,
  The flow -- ers that bloom in the spring.
  Tra la la la la,
  Tra la la la la,
  Tra la la la la la!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The flow -- ers that bloom in the spring, Tra la,
  Have no -- thing to do with the case,
  I’ve got to take un -- der my wing, Tra la,
  A most un -- at -- trac -- tive old thing, Tra la,
  With a car -- i -- ca -- ture of a face,
  With a car -- i -- ca -- ture of a face;
  
  And that’s what I mean when I say or I sing,
  “Oh both -- er the flow -- ers that bloom in the spring!”
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
  ees8 |
  ees8. ees16 ees8 ees8. ees16 ees8
  ees8 s aes aes s ees |
  ees8. des16 c8 des des des |
  c4. s4
  
  ees16~ ees |
  ees8. ees16 ees8 ees8. ees16 ees8
  ees8 s aes aes s ees |
  e8. e16 e8 e8. e16 e8 |
  f8 s aes aes s
  
  f16 f |
  d8. d16 d8 d8. d16 d8 |
  ees4. ees4 ees8 |
  ees8. ees16 ees8 d8. d16 d8 |
  ees4. s4
  
  ees8 |
  f g aes des c des |
  bes aes bes g4 ees8 |
  f g aes des c des |
  bes aes bes g4
  
  ees8 |
  ees8. ees16 ees8 ees4 c8 |
  ees8. ees16 ees8 ees4 c8 |
  ees8. des16 c8 des des des |
  c4. s4
  
  c8 |
  ees8. ees16 ees8 ees4 ees8 |
  f8. f16 f8 f8 r4 |
  aes8. aes16 aes8 g4 g8 |
  aes4. s4 \bar"|."
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
  c8 |
  c8. des16 c8 c8. des16 c8 |
  c8 s c c s aes |
  aes8. g16 aes8 g g g |
  aes4. s4
  
  c16~ c |
  c8. des16 c8 c8. des16 c8 |
  c8 s c c s aes |
  bes8. bes16 bes8 bes8. bes16 bes8 |
  aes8 s c c s
  
  aes16 aes |
  aes8. aes16 aes8 aes8. aes16 aes8 |
  g4. g4 ges8 |
  g?8. g16 g8 aes8. aes16 aes8 |
  g4. s4
  
  ees8 |
  f g aes des c des |
  bes aes bes g4 ees8 |
  f g aes des c des |
  bes aes bes g4
  
  des'8 |
  c8. c16 c8 c4 aes8 |
  c8. c16 c8 c4 aes8 |
  aes8. g16 aes8 g bes g |
  aes4. s4
  
  aes8 |
  c8. c16 c8 c4 c8 |
  c8. c16 c8 c8 s4 |
  f8. f16 f8 ees4 des8 |
  c4. s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes8 |
  aes8. aes16 aes8 aes8. aes16 aes8 |
  aes8 d,\rest aes' aes d,\rest aes |
  aes8. ees'16 aes,8 ees' ees ees |
  aes,4. d4\rest
  
  aes'16~ aes |
  aes8. aes16 aes8 aes8. aes16 aes8 |
  aes8 d,\rest aes' aes d,\rest aes' |
  g8. g16 g8 c,8. c16 c8 |
  f8 d\rest f f d\rest
  
  f16 f |
  f8. f16 f8 bes,8. bes16 bes8 |
  ees4. ees4 aes,8 |
  bes8. bes16 bes8 bes8. bes16 bes8 |
  ees4. d4\rest
  
  ees8 |
  f g aes des c des |
  bes aes bes g4 ees8 |
  f g aes des c des |
  bes aes bes g4\fermata
  
  g8 |
  aes8. aes16 aes8 aes4 aes8 |
  aes8. aes16 aes8 aes4 aes8 |
  aes,8. ees'16 aes,8 ees' ees ees |
  aes,4. d4\rest
  
  aes'8 |
  aes8. aes16 aes8 aes4 aes8 |
  f8. f16 f8 f8 d4\rest |
  des'8. c16 bes8 ees,4 ees8 |
  aes4. d,4\rest \bar"|."
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
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Flowers that Bloom in the Spring"}}
  poet = \markup\oldStyleNum"W. S. Gilbert (1836–1911)"
  composer = \markup\oldStyleNum"Arthur Sullivan (1842–1900)"
  tagline = ""
}}


