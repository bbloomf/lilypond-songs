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
  first-page-number = #171
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
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
	g'4\p g8 g bes4. bes8 |
  ees,2. b'4\rest |
  g4\p g8. g16 g8.~ g16 \cresc g8.~ g16 |
  
  g4\f a8. b16 c4( c,) |
  bes'4\p bes8. bes16 bes8.~ bes16 \cresc bes8.~ bes16 |
  bes4\f( c8.) d16 ees4( ees,8)~ ees |
  
  g4\p~ g8. g16 aes4\cresc~ aes8. aes16 |
  a4. a8 bes2 |
  ees4\f g,8. g16 aes4. d,8\p\fermata |
  ees2\fermata b'\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	Fa -- ther! I bend to Thee,
  Life, it was Thy __ _ gift, __ _
  Thou now canst shield it,
  From Thee it came, _ and to Thee __ _ I yield it,
  In life __ _ or death _ for -- sake not me,
  Fa -- ther, I bend to Thee!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Fa -- ther! I trust to Thee,
  When midst the bat -- tle’s strife, __ _
  Death did sur -- round me,
  E’en at the can -- non’s mouth, _
  Death has not found me. _
  Fa -- ther, ’twas Thy will! I trust in Thee.
  Fa -- ther, still guide Thou me!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  All I give back to Thee!
  When at Thy call, __ _ I my
  Life then shall yield, __ _
  When in the cold __ _ tomb, my fate shall be seal’d, __ _ _
  Fa -- ther, my soul __ _ take un -- to Thee!
  Fa -- ther, for -- sake not me!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees4 ees8 ees d4. d8 |
  ees2. s4 |
  d4 d8. d16 ees8.~ ees16 ees8.~ ees16 |
  
  f4 f8. f16 ees4( c) |
  f4 f8. f16 g8.~ g16 g8.~ g16 |
  aes4~ aes8. aes16 g4( ees8)~ ees |
  
  ees4~ ees8. ees16 ees4~ ees8. ees16 |
  ees4. ees8 \slurSolid ees4( d) |
  ees4 ees8. ees16 d4. bes8 |
  bes2 s \bar"|."
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
  bes4 bes8 bes aes4. aes8 |
  g2. s4 |
  b4 b8. b16 c8.~ c16 c8.~ c16 |
  
  d4 d8. d16 c4~ c |
  d4 d8. d16 ees8.~ ees16 ees8.~ ees16 |
  f4~ f8. f16 ees4~ ees8~ ees |
  
  bes4~ bes8. bes16 aes4~ aes8. aes16 |
  f4. f8 f2 |
  g4 bes8. g16 f4. aes8 |
  g2 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 ees8 ees bes4. bes8 |
  c2. d4\rest |
  g4 g8. g16 g8.~ g16 g8.~ g16 |
  
  g,4 g8. g16 c4~ c |
  bes'4 bes8. bes16 bes8.~ bes16 bes8.~ bes16 |
  bes,4~ bes8. bes16 ees4~ ees8~ ees |
  
  des4~ des8. des16 c4~ c8. c16 |
  c4. c8 bes2 |
  bes4 bes8. bes16 bes4. bes8\fermata |
  <ees ees,>2 d\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
  \key ees \major
  \time 4/4
  \slurSolid \tieSolid
  <bes ees g>4 q8.[ q16] <d f bes>4 q |
  <ees g,>2. r4 |
  <g b d>2 <c ees> |
  << {g'4. (f8 ees4)} \\ {<d b g>2 <c g>4} >> r4 |
  <bes? d f>2 <bes ees g> |
  << {bes'4.( aes8 g4)} \\ {<f d bes>2 <ees bes>4} >> r4 |
  
  <g, ees des>2 <aes ees c> |
  <a ees c> <bes d, bes>8 <d f>[( <ees g> <f aes>]) |
  <g ees bes>2\arpeggio <aes f d bes>\arpeggio |
  <g ees bes>4 q8.[ q16] <bes d, bes>4 <d, bes aes>( |
  <ees bes g>) <g, ees bes>8.[ q16] <aes f d bes>4 <d, aes>^( |
  <ees g,>4) <g bes,> <ees g,>2 \bar"||" \break
}
pianoLH = \relative c' {
  \key ees \major
  \time 4/4
  \slurSolid \tieSolid
  <ees, ees,>2 <bes bes,> |
  <c c,>2. r4 |
  <g g'>2 q |
  q_( <c g'>4) r |
  <bes bes'>2 q |
  q_( <ees bes'>4) r |
  
  q2 <aes aes,> |
  <f f,>2 <f bes,>8 r r4 |
  <bes bes,>2 q |
  <bes ees,>4 q <bes bes,> q |
  <bes ees,> <ees, ees,> <bes bes,>2 |
  <ees bes ees,>4 q q2 \bar"||"
}

pianoWords = \lyricmode {
  \override LyricText #'font-size = #0
  \markup\dynamic f "" ""
  \markup\dynamic p \markup\italic cresc. \markup\dynamic fz
  \repeat unfold 2 \skip1
  \markup\dynamic fz
  \markup\dynamic p
  \markup\italic cresc.
  \markup\dynamic f \set associatedVoice = "pianoRH" "" \markup\dynamic p
  \unset associatedVoice "" \markup\dynamic pp
  "" \repeat unfold 7 \skip1
  \markup\dynamic pp
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women {
      \global \new Voice = "pianoRH" {\set midiInstrument = #"acoustic grand" \pianoRH}
      <<
        \new Voice = "sopranos" { \voiceOne << \global \set midiInstrument = #"flute" \sopMusic >> }
        \new Voice = "altos" { \voiceTwo << \global \set midiInstrument = #"flute" \altoMusic >> }
      >>
    }
    \new Lyrics = "pianos"
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men {
     \clef bass
     \global \new Voice = "pianoLH" {\set midiInstrument = #"acoustic grand" \pianoLH}
     <<
        \new Voice = "tenors" { \voiceOne << \global \set midiInstrument = #"flute" \tenorMusic >> }
        \new Voice = "basses" { \voiceTwo << \global \set midiInstrument = #"flute" \bassMusic >> }
      >>
    }
    \context Lyrics = "pianos"  \lyricsto "pianoLH" \pianoWords
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Battle Prayer"}}
  composer = \markup\oldStyleNum"Friedrich Heinrich Himmel (1765–1814)"
  tagline = ""
}}


