\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Red Wing"}}
  composer = \markup\oldStyleNum"Kerry Mills (1869–1948)"
  poet = \markup\oldStyleNum"Thurland Chattaway"
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
  But brave and __ _ gay, he rode one _ day to bat -- tle far __ _ a -- way.
  
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
  For far, far a -- way, her war -- rior _ gay, fell brave -- ly in __ _ the fray.
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
  a8 a bes aes |
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
  ees ees16[ d] ees8 ees |
  f f16[ ees] d8 d |
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
