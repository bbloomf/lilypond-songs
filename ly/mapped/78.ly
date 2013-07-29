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
       (padding . -1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 100))
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
  first-page-number = #78
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
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  bes'8 |
  bes8. c16 bes8 g8. f16 ees8 |
  
  c'4. b8\rest ees8. c16 |
  bes8. c16 bes8 g8. f16 ees8 |
  f4.~ f8 b\rest \bar""\break bes |
  
  bes8. c16 bes8 g8. f16 ees8 |
  c'4.~ c8 b\rest \tieDashed ees16~ ees |
  \tieSolid bes8. g16 ees8 f8. ees16 f8 |
  ees4.~ ees8 b'\rest \bar""\break ees, |
  
  %page2/77
  c'8. b16 c8 ees8. d16 c8 |
  bes g8. aes16 bes4\( b8\) |
  c8. b16 c8 ees8. d16 c8 |
  d4.~ d8 b\rest \bar""\break bes |
  
  bes8. c16 bes8 g8. f16 ees8 |
  c'4.~ c8 bes\rest ees |
  bes16 g8. ees8 f8. ees16 f8 |
  ees4.~ ees8 b'\rest \bar""\break
  
  %Chorus
  ees,8 |
  c'8. b16 c8 ees8. d16 c8 |
  bes4.~ bes8 b\rest b |
  c8. b16 c8 ees8. d16 c8 |
  d4.~ d8 b\rest \bar""\break bes |
  
  bes8. c16 bes8 g8. f16 ees8 |
  c'4.~ c8 b\rest ees |
  bes16 g8. ees8 f8. ees16 f8 |
  ees4.~ ees8 b'\rest \bar""\break c |
  
  c8. bes16 aes8 g\fermata b\rest bes |
  bes c d ees\fermata b\rest c |
  bes16 g8. ees8 f8. ees16 f8 |
  ees4.~ ees4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	I’m think -- ing of Er -- in to -- night,
  And the lit -- tle white cot by the sea, __
  Where Jen -- ny my dar -- ling now dwells,
  The __ fair -- est and dear -- est to me; __
  
  I know that she waits for me day af -- ter day,
  My heart ev -- er longs to be there,
  To meet her, my dar -- ling, my own, __
  Sweet Jen -- ny, the flow’r of Kil -- dare. __
  
  I know that she’s wait -- ing for me, __
  My heart ev -- er longs to be there; __
  To meet her, my dar -- ling, my own, __
  Sweet Jen -- ny, the flow’r of Kil -- dare, __
  The flow’r of Kil -- dare,
  The flow’r of Kil -- dare,
  Sweet Jen -- ny, the flow’r of Kil -- dare, __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I’m wait -- ing her sweet face to see,
  While we’re part -- ed I lin -- ger in pain,
  But soon will my heart beat with joy, __
  \set ignoreMelismata = ##t
  O’er the sea \unset ignoreMelismata I’ll be sail -- ing a -- gain; __
  
  A -- gain her sweet kiss -- es of love to re -- ceive,
  For her the sea’s storms I will dare,
  To meet her, my dar -- ling, my own, __
  Sweet Jen -- ny, the flow’r of Kil -- dare. __
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
  ees8. ees16 ees8 ees8. ees16 ees8 |
  ees4. s8 ees8. ees16 |
  ees8. ees16 ees8 ees8. ees16 c8 |
  d4.~ d8 s ees |
  ees8. ees16 ees8 ees8. ees16 ees8 |
  
  ees4.~ ees8 s \tieDashed ees16~ ees |
  \tieSolid
  ees8. ees16 ees8 d8. bes16 d8 |
  ees4.~ ees8 s c |
  ees8. d16 ees8 ees8. ees16 ees8 |
  ees8 ees8. d16 ees4 ees8 |
  
  ees8. d16 ees8 f8. f16 f8 |
  f4.~ f8 s f |
  ees8. ees16 ees8 ees8. ees16 ees8 |
  ees4.~ ees8 s ees |
  ees16 ees8. ees8 d8. bes16 d8 |
  
  ees4.~ ees8 s
  
  c8 |
  ees8. d16 ees8 ees8. ees16 ees8 |
  ees4.~ ees8 s ees |
  ees8. d16 ees8 f8. f16 f8 |
  f4.~ f8 s f |
  
  ees8. ees16 ees8 ees8. ees16 ees8 |
  ees4.~ ees8 s ees |
  ees16 ees8. bes8 bes8. bes16 bes8 |
  bes4.~ bes8 s ees |
  
  ees8. d16 c8 bes8 s bes |
  bes a aes g s ees' |
  ees16 ees8. bes8 bes8. bes16 bes8 |
  bes c8. c16 bes4 \bar"|."
}
altoWords = \lyricmode {
  \repeat unfold 118 ""
  of Kil -- dare
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
  g8 |
  g8. aes16 g8 bes8. aes16 g8 |
  aes4. s8 c8. aes16 |
  g8. aes16 g8 bes8. aes16 a8 |
  bes4.~ bes8 s g |
  g8. aes16 g8 bes8. aes16 g8 |
  
  aes4.~ aes8 s \tieDashed c16~ c |
  \tieSolid g8. bes16 g8 aes8. g16 aes8 |
  g4.~ g8 s aes |
  aes8. aes16 aes8 c8. bes16 aes8 |
  g8 bes8. bes16 g4 g8 |
  
  aes8. aes16 aes8 a8. a16 a8 |
  bes4.~ bes8 s aes? |
  g8. aes16 g8 bes8. aes16 g8 |
  aes4.~ aes8 s c |
  g16 bes8. g8 aes8. g16 aes8 |
  g4.~ g8 s
  
  aes8 |
  aes8. aes16 aes8 c8. bes16 aes8 |
  g4.~ g8 s g |
  aes8. aes16 aes8 a8. bes16 a8 |
  bes4.~ bes8 s aes? |
  
  g8. aes16 g8 bes8. aes16 g8 |
  aes4.~ aes8 s c |
  g16 bes8. g8 aes8. g16 aes8 |
  g4.~ g8 s s |
  
  s2. |
  s2 s8 aes |
  g16 bes8. g8 aes8. g16 aes8 |
  g8 aes8. aes16 g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees8. ees16 ees8 ees8. ees16 ees8 |
  aes,4. d8\rest aes8. aes16 |
  ees'8. ees16 ees8 ees8. ees16 f8 |
  bes,4.~ bes8 d\rest ees |
  ees8. ees16 ees8 ees8. ees16 ees8 |
  
  aes,4.~ aes8 d\rest \tieDashed aes16~ aes |
  \tieSolid
  bes8. bes16 bes8 bes8. bes16 bes8 |
  ees4.~ ees8 d\rest aes |
  aes8. aes16 aes8 aes8. aes16 aes8 |
  ees'8 ees8. f16 ees4 ees8 |
  
  aes,8. aes16 aes8 f'8. f16 f8 |
  bes,4.~ bes8 d\rest bes |
  ees8. ees16 ees8 ees8. ees16 ees8 |
  aes,4.~ aes8 d\rest aes |
  bes16 bes8. bes8 bes8. bes16 bes8 |
  
  ees4.~ ees8 d\rest
  
  aes8 |
  aes8. aes16 aes8 aes8. aes16 aes8 |
  ees'4.~ ees8 d\rest ees |
  aes,8. aes16 aes8 f'8. f16 f8 |
  bes,4.~ bes8 d\rest bes |
  
  ees8. ees16 ees8 ees8. ees16 ees8 |
  aes,4.~ aes8 d\rest aes |
  bes16 bes8. bes8 bes8. bes16 bes8 |
  ees4.~ ees8 d\rest d\rest |
  
  d2.\rest\fermata |
  d4\rest d8\rest d\rest\fermata d\rest aes |
  bes16 bes8. bes8 bes8. bes16 bes8 |
  ees ees8. ees16 ees4 \bar"|."
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
    \new Lyrics \lyricsto "altos" \altoWords
    \new Lyrics = "sop"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics \lyricsto "altos" \altoWords
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
      %\Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Jenny the Flower of Kildare"}}
  poet = \markup\oldStyleNum"Frank Dumont "
  composer = \markup\oldStyleNum"James E. Stewart (b. 1843)"
  tagline = ""
}}


global = {
  \key c \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	c'2 b4 b |
  a a g g |
  f f e c |
  f g c,2 \bar"||"
}
sopWords = \lyricmode {
  Come fol -- low, fol -- low, fol -- low,
  Fol -- low, fol -- low, fol -- low me!
}

sopWordsII = \lyricmode {
  Whith -- er shall I fol -- low, fol -- low, fol -- low,
  Whith -- er shall I fol -- low, fol -- low thee?
}

sopWordsIII = \lyricmode {
  To the gal -- lows, to the gal -- lows,
  To the gal -- lows, gal -- lows tree.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 d e f g4 e |
  f d e c |
  f8 g a b c4 c |
  c b c2 \bar"||"
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
  e'4. e8 d4 e |
  c4. d8 b4 c |
  a4. a8 g4 e' |
  d4. c8 c2 \bar"||"
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
    \new Staff = women <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Staff = women <<
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "altosIII"  \lyricsto "tenors" \sopWordsIII
    
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Come Follow"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Round)"}}
  tagline = ""
}}


global = {
  \key g \major
  \time 6/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  d'4 |
  b4. c8 d4 e e d |
  d c b a2 b4 |
  c c b a4. g8 a4 |
  b2.~ b2 \bar""
}
sopWords = \lyricmode {
  Come fol -- low me mer -- ri -- ly, mer -- ri -- ly
  Lads come fol -- low me mer -- ri -- ly, ah:
}

sopWordsII = \lyricmode {
  %And we will sing sol fa fa sol fa fa fa sol sol fa.
   And we will sing sol do do sol do fa do sol sol do.
}

sopWordsIII = \lyricmode {
  %Put sol be -- fore La and Fa af -- ter Mi sol La mi fa mi La mi fa.
   Put sol be -- fore La and Do af -- ter Ti sol La ti do ti La ti do.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'4 |
  g2 d4 a'2 d,4 |
  g2 g4 d2 g4 |
  c,2 g'4 d2 d4 |
  g2. b2\rest \bar""
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
  b'4 |
  d4. e8 fis4 e2 fis4 |
  g g g fis2 d4 |
  e4. fis8 g4 fis4. e8 fis4 |
  g2.~ g2 \bar""
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
    \new Staff = women <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Staff = women <<
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "altosIII"  \lyricsto "tenors" \sopWordsIII
    
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Come Follow Me Merrily"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Round)"}}
  tagline = ""
}}


