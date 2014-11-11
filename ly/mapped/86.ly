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
       (padding . 0)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #86
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
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8 bes8
  ees ees16 d ees8 g |
  
  bes4. ees,8 |
  aes aes16 c ees8 c |
  bes4. g8 |
  
  aes aes16[ g] f8 aes |
  g g16[ f] ees8 g |
  f c d ees |
  
  %page 2
  f16 g f d bes8 bes |
  ees ees16[ d] ees8 g |
  bes4. ees,8 |
  
  aes8 aes16[ c] ees8 c |
  bes4. g8 |
  aes aes16[ g] f8 aes |
  
  g g16[ f] ees8 g |
  f c d16[ c] bes8 |
  ees8 b'\rest \bar"||" \break
  
  %chorus
  ees8 d |
  c4 c8. b16 |
  c8 ees d c |
  
  bes4 g~ |
  g8 bes c bes |
  bes4 f~ |
  f8 bes c bes |
  bes4 g~ |
  g ees'8 d |
  c4 c8. b16 |
  c8 ees d c |
  
  bes4 g~ |
  g8 bes c bes |
  bes4 f~ |
  f8 aes g f |
  ees2~ |
  ees8 bes'\rest bes\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	There once lived an In -- dian maid,
  A shy lit -- tle prai -- rie maid,
  Who sang a __ _ lay, a love song __ _ gay,
  As on the plain she’d while a -- way the day;
  
  She loved a __ _ war -- rior bold,
  this shy lit -- tle maid of old,
  But brave and __ _ gay, he rode one __ _ day to bat -- tle far __ _ a -- way.
  
  Now, the moon shines to -- night on pret -- ty Red Wing, __ _
  The breeze is sigh -- ing, __ _ the night bird’s cry -- ing, __ _
  For a -- far ’neath his star her brave is sleep -- ing, __ _
  While Red Wing’s weep -- ing __ _ her heart a -- way. __ _
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  She watched for him day and night,
  She kept all the camp -- fires bright,
  And un -- der the sky, each night she would lie,
  And dream a -- bout his com -- ing by and by;
  
  But when all the braves re -- turned,
  the heart of __ _ Red Wing yearned,
  For far, far a -- way, her war -- rior __ _ gay, fell brave -- ly in __ _ the fray.
}

sopWordsIII = \lyricmode {
}

sopWordsIV = \lyricmode {
}

sopWordsV = \lyricmode {
}

altoMusic = \relative c' {
  \partial 8
  bes8 |
  bes ees16 bes ees8 ees |
  des4. ees8 |
  c8 c16 ees c8 ees |
  ees4. ees8 |
  
  f f16[ ees] d8 f |
  ees ees16[ bes] c8 ees |
  c c bes c |
  bes16 ees bes bes bes8 aes
  
  
  bes ees16[ bes] ees8 ees |
  des4. ees8 |
  c8 c16[ ees] c8 ees |
  ees4. ees8 |
  
  f f16[ ees] d8 f |
  ees ees16[ bes] c8 ees |
  c c bes bes |
  bes s \bar"||"
  
  %Chorus
  ees f |
  ees4 ees8. ees16 |
  ees8 ees f ees |
  ees4 ees~ |
  ees8 ees ees ees |
  
  d4 d~ |
  d8 d d d |
  ees4 bes~ |
  bes ees8 f |
  
  ees4 ees8. ees16 |
  ees8 ees f ees |
  ees4 ees~ |
  ees8 ees ees ees |
  d4 d~ |
  d8 d d d |
  bes2~ |
  bes8 s4 \bar"|."
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
  \partial 8 bes8 |
  g bes16 bes g8 bes |
  g4. g8 |
  ees8 ees16 aes aes8 aes |
  g4. bes8 |
  
  bes8 bes16[ bes] bes8 bes |
  bes8 bes16[ bes] g8 c |
  a8 a bes a |
  bes16 bes bes f d8 d
  
  
  g bes16[ bes] g8 bes |
  g4. g8 |
  ees8 ees16[ aes] aes8 aes |
  g4. bes8 |
  
  bes8 bes16[ bes] bes8 bes |
  bes8 bes g c |
  a8 a f aes |
  g s \bar "||"
  
  %Chorus
  g f |
  aes4 aes8. aes16 |
  aes8 c bes aes |
  g4 bes~ |
  bes8 g aes g |
  
  aes4 aes~ |
  aes8 aes aes aes |
  g4 ees~ |
  ees g8 f |
  
  aes4 aes8. aes16 |
  aes8 c bes aes |
  g4 bes~ |
  bes8 g aes g |
  
  aes4 aes~ |
  aes8 c bes aes |
  g2~ |
  g8 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 8 bes8 |
  ees,8 g16 f ees8 ees |
  
  ees4. ees8 |
  aes,8 aes16 aes aes8 c |
  ees4. ees8 |
  
  d8 d16[ ees] bes8 d |
  ees ees16[ d] c8 c |
  f f f f |
  
  d16 ees d bes bes8 bes |
  ees8 g16 f ees8 ees |
  
  ees4. ees8 |
  aes,8 aes16[ aes] aes8 c |
  ees4. ees8 |
  
  d8 d16[ ees] bes8 d |
  ees ees16[ d] c8 c |
  f f16[ ees] bes8 d |
  ees8 d\rest \bar"||"
  
  %Chorus
  ees8 bes |
  aes4 aes8. aes16 |
  aes8 aes bes c |
  ees4 <ees ees,>4~ |
  q8 ees ees ees |
  
  f4 bes,~ |
  bes8 f' f f |
  ees4 ees~ |
  ees ees8 bes |
  
  aes4 aes8. aes16 |
  aes8 aes bes c |
  ees4 <ees ees,>~ |
  q8 ees ees ees |
  f4 bes,~ |
  bes8 bes bes bes |
  ees2~ |
  ees8 d\rest d\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Red Wing"}}
  composer = \markup\oldStyleNum"Kerry Mills (1869–1948)"
  poet = \markup\oldStyleNum"Thurland Chattaway"
  tagline = ""
}}
global = {
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
  \tieDashed
}

sopMusic = \relative c' {
	\partial 4
  d8 g |
  b4 b8 a g4 a8 g |
  e g4. b4\rest \bar"" d,8 g |
  b4 g8 b d4 c8 b |
  a2. \bar"" d8 c |
  b4 b8 a g4 a8 b |
  d c4. b4\rest \bar"" e,8 ees |
  d4 fis8 g a4 b8 a |
  g2. \bar":|."
  %\break
  
  
  d8 g |
  b4 b8 a g4 a8 g |
  e g4. b4\rest \bar"" d,8 g |
  b4 g8 b d4 c8 b |
  a2. \bar"" \break d8 c |
  b4 b8 a g4 a8 b |
  d c4. b4\rest \bar"" e,8 ees |
  d4 fis8 g a4 b8 a |
  g2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  %\set ignoreMelismata = ##t
	From this val -- ley they say you are go -- ing.
  We will miss your bright eyes and sweet smile,
  For they say you are tak -- ing the sun -- shine
  That has bright -- ened our path -- way a while.
}

sopWordsII = \lyricmode {
  %\set stanza = #"Chorus "
  %\set ignoreMelismata = ##t
  I’ve been think -- ing a long time, my dar -- ling!
  Of the sweet words you nev -- er would say,
  Now a -- las, must the fond hopes all van -- ish?
  For they say you are go -- ing a -- way.
  
  
  
  \dropLyricsXI
  Come and sit by my side if you love me.
  Do not hast -- en to bid me a -- dieu.
  \raiseLyrics
  Just re -- mem -- ber the Red Riv -- er Val -- ley,
  And the cow -- boy who loved you so true.
}

sopWordsIII = \lyricmode {
  \set stanza = "2. "
  I have prom -- ised you dar -- ling that nev -- er,
  Shall a word from my lips cause you pain
  And my life it will be yours for -- ev -- er,
  If you on -- ly will love me a -- gain.
}

sopWordsIV = \lyricmode {
  Won’t you think of the val -- ley you’re leav -- ing,
  Oh! how lone -- ly and drear it will be,
  Won’t you think of the fond heart you’re break -- ing,
  And the pain you are caus -- ing to me.
}

sopWordsV = \lyricmode {
}

altoMusic = \relative c' {
  b8 b |
  d4 d8 c d4 f8 f |
  e8 e4. s4 d8 d |
  
  d4 d8 d g4 g8 g |
  \slurSolid fis4( g fis) g8 e |
  g4 g8 d d4 f8 f |
  e8 e4. s4 bes8 bes |
  b?4 d8 d d4 g8 fis |
  d4( e d) \bar":|."
  
  
  
  b8 b |
  d4 dis8 dis e4 f8 f |
  e e4. s4 d8 d |
  
  d4 d8 g gis4 gis8 gis |
  g?2( fis4) g8 e |
  g4 g8 d d4 f8 f |
  
  e e4. s4 bes8 bes |
  b?4 d8 d d4 g8 fis |
  d4( e d) \bar"|."
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
  d,8 d |
  g4 g8 g b4 b8 b |
  g8 c4. s4 g8 b |
  
  g4 b8 g b4 e8 d |
  c2. b8 c |
  d4 d8 c b4 b8 g |
  
  g8 g4. s4 g8 g |
  g4 a8 b c4 d8 c |
  \slurSolid b4( c b) \bar":|."
  
  
  
  g8 g |
  g4 a8 b b4 b8 b |
  g c4. s4 g8 b |
  
  g4 g8 b b4 e8 d |
  cis2( c4) b8 c |
  d4 d8 c b4 b8 g |
  
  g g4. s4 g8 g |
  g4 a8 b c4 d8 c |
  b4( c b) \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g,8 g |
  g4 g8 g g4 g8 g |
  c c4. d4\rest b8 g |
  
  g4 g'8 g g4 g8 g |
  \slurSolid d4( e8[ ees] d4) g8 g |
  g4 g8 g g4 g,8 g |
  
  c c4. d4\rest cis8 cis |
  d4 d8 d d4 d8 d |
  g2. \bar":|."
  
  
  
  g8 g |
  g4 fis8 fis e4 d8 d |
  c c4. d4\rest b8 g |
  
  g4 f'8 f e4 e8 e |
  \slurSolid e4( ees d) g8 g |
  g4 g8 g g4 g,8 g |
  
  c8 c4. d4\rest cis8 cis |
  d4 d8 d d4 d8 d |
  g2. \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Red River Valley"}}
  composer = \markup\oldStyleNum"Traditional"
  poet = \markup\oldStyleNum"Traditional"
  tagline = ""
}}
